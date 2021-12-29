#Install packages ----
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("lme4")
#load packages
library(tidyverse)
#library(nflfastR)
library(lme4)

#Import Data ----
PFFScout = read.csv("PFFScoutingData.csv")
games = read.csv("games.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")
plays = rename(plays, nflId = returnerId)
plays$nflId = as.factor(plays$nflId)

#Merging Scout data & play data ----
DataMerge = merge(plays,PFFScout, by= c("gameId","playId"), all =TRUE)


KickOffData = DataMerge %>%
  filter(specialTeamsPlayType == "Kickoff") %>% #returning only kickoffs
  filter(specialTeamsResult == "Return") #returning only plays that have a return 
toMatch = c("onside", "MUFFS", "FUMBLES", "Lateral","Touchback") #making sure nothing gets through the prior filter
KickOffData =  KickOffData[-grep(paste(toMatch,collapse="|"), KickOffData$playDescription), ]
KickOffData = KickOffData %>% 
  left_join(games, by=c("gameId"))%>% 
  drop_na(season, week, gameDate, gameTimeEastern)

KickOffData$scoreDifferential = case_when(KickOffData$possessionTeam == KickOffData$homeTeamAbbr ~ KickOffData$preSnapVisitorScore - KickOffData$preSnapHomeScore, 
                                              TRUE ~ KickOffData$preSnapHomeScore - KickOffData$preSnapVisitorScore)
KickOffData$scoreDifferential= case_when(KickOffData$scoreDifferential >= 27 ~ 27, TRUE ~ as.numeric(KickOffData$scoreDifferential))
KickOffData$scoreDifferential= case_when(KickOffData$scoreDifferential <= -27 ~ -27, TRUE ~ as.numeric(KickOffData$scoreDifferential))


KickOffData = select(KickOffData, gameId, playId, playDescription, quarter, possessionTeam, nflId, kickReturnYardage, scoreDifferential, homeTeamAbbr, visitorTeamAbbr, absoluteYardlineNumber)


#pull kickoffs that are caught outside of the hashmarks and are returned ----
tracking2018 = read.csv("tracking2018.csv")
tracking2019 = read.csv("tracking2019.csv")
tracking2020 = read.csv("tracking2020.csv")

tracking_data_used = rbind(tracking2018, tracking2019, tracking2020) %>%
  dplyr::filter(gameId %in% KickOffData$gameId) %>%
  dplyr::filter(playId %in% KickOffData$playId)

#trying to find max after a return
tracking_data_used = tracking_data_used %>%
  group_by(gameId, playId, displayName) %>% 
  mutate(after_catch = cumany(event == "kick_received"))

max.x = tracking_data_used %>%
  filter(displayName == "football") %>%
  filter(after_catch == "TRUE")%>%
  group_by(gameId, playId)%>%
  slice(which.max(x)) %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(max.x = x, max.y =y)
  
min.x = tracking_data_used %>%
  filter(displayName == "football") %>%
  filter(after_catch == "TRUE")%>%
  group_by(gameId, playId)%>%
  slice(which.min(x)) %>%
  select(gameId, playId, x, y, frameId)%>%
  rename(min.x = x, min.y =y)

tracking_data_used = as.data.frame(tracking_data_used)

#all plays where a ball is caught
tracking_kickoff = tracking_data_used %>%
  dplyr::filter(event == "kickoff") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId,team) %>%
  dplyr::rename(x_kickoff = x, y_kickoff = y, event_kickoff = event, displayName_kickoff = displayName, frameId_kickoff = frameId) 

#all plays where a ball is caught
tracking_kick_received = tracking_data_used %>%
  dplyr::filter(event == "kick_received") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) %>%
  dplyr::rename(x_received = x, y_received = y, event_received = event, displayName_received = displayName, frameId_received = frameId) 

#plays that ended with a tackle, touchdown, or ran out of bounds
tracking_play_end = tracking_data_used %>%
  dplyr::filter(event =="tackle" | event =="touchdown" | event =="out_of_bounds") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) %>%
  dplyr::rename(x_play_end = x, y_play_end = y, event_play_end = event, displayName_play_end = displayName, frameId_play_end = frameId)


all_returns = tracking_kick_received %>% 
  left_join(tracking_play_end, by=c("gameId", "playId")) %>%
  left_join(max.x, by=c("gameId", "playId")) %>% 
  left_join(min.x, by=c("gameId", "playId")) %>%
  dplyr::filter(!between(y_received, 23.3, 29.9))%>%
  drop_na(y_play_end) #remove kick in the hashes removes around 400 returns

all_returns$crosses = case_when((all_returns$x_received <=30 & all_returns$y_received <= 23.3 & all_returns$max.x >=29.9 ~ TRUE),
                                              all_returns$x_received <= 30 & all_returns$y_received >= 29.9 & all_returns$min.x <=23.3 ~ TRUE,
                                              all_returns$x_received >= 70 & all_returns$y_received <= 23.3 & all_returns$max.x >=29.9 ~ TRUE,
                                              all_returns$x_received >= 70 & all_returns$y_received >= 23.9 & all_returns$min.y <=23.3 ~ TRUE,
                                              TRUE~FALSE)

all_returns = all_returns %>% 
  left_join(KickOffData, by=c("gameId", "playId"))
all_returns = all_returns %>% 
  drop_na(playDescription)

all_returns$side_of_field_received = case_when(all_returns$y_received >= 29.9 ~ "Right", TRUE ~ "Left")

all_returns$side_of_field_received = case_when((all_returns$x_received >= 50 & all_returns$side_of_field_received == "Right" ~ "Left"),
                                      (all_returns$x_received >= 50 & all_returns$side_of_field_received == "Left" ~ "Right"),
                                      (all_returns$x_received <= 50 & all_returns$side_of_field_received == "Left" ~ "Left"),
                                      (all_returns$x_received <= 50 & all_returns$side_of_field_received == "Right" ~ "Right"))

all_returns$returnTeam = case_when(all_returns$possessionTeam == all_returns$homeTeamAbbr ~ all_returns$visitorTeamAbbr, TRUE ~ all_returns$homeTeamAbbr)
#join some player data to match to nflIDs
all_returns = all_returns %>% 
  left_join(players, by=c("nflId"))

#creating fillerId to assign to returners that have had less than 5 returns
return_count_by_Id = count(all_returns, nflId)
return_count_by_Id$Id = case_when(return_count_by_Id$n <= 5 ~ "filler_Id", TRUE ~ as.character(return_count_by_Id$nflId))
#returning ID class from character back to factor
return_count_by_Id$Id =  as.factor(return_count_by_Id$Id)

all_returns = all_returns %>% 
  left_join(return_count_by_Id, by=c("nflId"))

#separating each return type ----
no_cross_return = all_returns %>%
  dplyr::filter(crosses == "FALSE")

cross_returns = all_returns %>%
  dplyr::filter(crosses == "TRUE")  

right_return = all_returns %>%
  dplyr::filter(side_of_field_received == "Right")

left_return = all_returns %>%
  dplyr::filter(side_of_field_received == "Left") 

mean(no_cross_return$kickReturnYardage)
mean(cross_returns$kickReturnYardage)

ggplot(all_returns, aes(crosses, kickReturnYardage))+
  geom_boxplot()

mean(left_return$kickReturnYardage)
mean(right_return$kickReturnYardage)

ggplot(all_returns, aes(side_of_field_received, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(crosses, side_of_field_received, fill= kickReturnYardage)) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="black") 

# t test for difference in crossing
t.test(right_return$kickReturnYardage,left_return$kickReturnYardage)
# there is a difference on the side of field

# t test for side of the field we start on
t.test(no_cross_return$kickReturnYardage,cross_returns$kickReturnYardage)
#there is a difference for cross vs no cross

#couple of diff models ----
#simple linear regression just to see what we're working with
summary(lm(kickReturnYardage ~ crosses + side_of_field_received + Id + scoreDifferential, data = all_returns))
#mixed model with crosses as the random effect
fit_mixed = lmer(kickReturnYardage ~ crosses + scoreDifferential + side_of_field_received + (crosses | Id), data = all_returns)
summary(fit_mixed)
ranef(fit_mixed)$Id %>% head(5)
coef(fit_mixed)$Id %>% head(5)

#Player influence area function ----

fit_mixed = lmer(kickReturnYardage ~ crosses + scoreDifferential + side_of_field + (crosses | Id), data = all_returns_after_1_second)
summary(fit_mixed)





