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
plays = rename(plays, nflId = returnerId )
tracking2018 = read.csv("tracking2018.csv")
tracking2019 = read.csv("tracking2019.csv")
tracking2020 = read.csv("tracking2020.csv")

#label years to keep track of when each play happened
tracking2018$year = 2018
tracking2019$year = 2019
tracking2020$year = 2020
tracking = rbind(tracking2018, tracking2019, tracking2020)

#Merging Scout data & play data ----
DataMerge = merge(plays,PFFScout, by= c("gameId","playId"), all =TRUE)

#Subsetting kickoff data
KickOffData = subset(DataMerge, specialTeamsPlayType == "Kickoff")
# remove safety, onsides, and squibs kicks. Basically only keeping normal kicks
KickOffData = subset(KickOffData, !(kickType %in% c("K", "O", "Q", "S")))
KickOffData = KickOffData[-grep("onside", KickOffData$playDescription), ]

#pull kickoffs that are caught outside of the hashmarks and are returned ----
KickOffData1 = select(KickOffData, gameId, playId, kickReturnYardage, nflId)

#all plays where there is a kickoff ----
trackingKickoff = tracking %>%
  dplyr::filter(event == "kickoff") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, event)

#all plays where a ball is caught (includes punts, kickoffs, safeties)
trackingKick_received = tracking %>%
  dplyr::filter(event == "kick_received") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) 

#plays that ended with a tackle, touchdown, or ran out of bounds
trackingEvent2 = tracking %>%
  dplyr::filter(event =="tackle" | event =="touchdown" | event =="out_of_bounds") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId)

#joining dfs to make sure we only get the parameters we want (ie a kickoff that has a clean catch, and ends with a tackle, touchdown, or out of bounds) ----
all_returns = trackingKickoff %>% 
  left_join(trackingKick_received, by=c("gameId", "playId")) %>% 
  left_join(trackingEvent2, by=c("gameId", "playId")) %>%
  drop_na(event.x, event.y) %>% 
  drop_na(x.y, y.y)

#label returns caught outside of the hashes that go cross field (i.e. midline)
all_returns = all_returns %>% 
  dplyr::filter(!between(y.x, 23.3, 29.9))  %>% 
  mutate(crosses = sign(y.x - 26.67) != sign(y.y - 26.67))

#drop_na removes fumbles and muffs that aren't recovered by the initial returner
all_returns = all_returns %>% 
  left_join(KickOffData1, by=c("gameId", "playId"))%>% 
  drop_na(kickReturnYardage)

all_returns$side_of_field = case_when(all_returns$y.x >= 29.9 ~ "Right", TRUE ~ "Left")

#remove plays that were caught before the 30, there was some mislabling with playdata so it was easiest to just remove these plays
all_returns = all_returns %>% 
  dplyr::filter(!between(x.x, 30, 70))

#change characters to factors
all_returns$nflId = as.factor(all_returns$nflId)
all_returns$crosses = as.factor(all_returns$crosses)
all_returns$side_of_field =  as.factor(all_returns$side_of_field)
players$nflId =  as.factor(players$nflId)

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

all_returns = all_returns %>%
  dplyr::select(-c(displayName.y, height, weight, birthDate, collegeName, Position, n))
#taking into account which direction the ball is kicked
all_returns$side_of_field = case_when((all_returns$x.x >= 50 & all_returns$side_of_field == "Right" ~ "Left"),
                                      (all_returns$x.x >= 50 & all_returns$side_of_field == "Left" ~ "Right"),
                                      (all_returns$x.x <= 50 & all_returns$side_of_field == "Left" ~ "Left"),
                                      (all_returns$x.x <= 50 & all_returns$side_of_field == "Right" ~ "Right"))

#separating each return type ----
no_cross_return = all_returns %>%
  dplyr::filter(crosses == "FALSE")

cross_returns = all_returns %>%
  dplyr::filter(crosses == "TRUE")  

right_return = all_returns %>%
  dplyr::filter(side_of_field == "Right")

left_return = all_returns %>%
  dplyr::filter(side_of_field == "Left") 

mean(no_cross_return$kickReturnYardage)
mean(cross_returns$kickReturnYardage)

ggplot(all_returns, aes(crosses, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(side_of_field, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(crosses, side_of_field, fill= kickReturnYardage)) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="black") 

# t test for difference in crossing
t.test(right_return$kickReturnYardage,left_return$kickReturnYardage)
# there is a difference on the side of field

# t test for side of the field we start on
t.test(no_cross_return$kickReturnYardage,cross_returns$kickReturnYardage)
#there is a difference on the side of the field

#couple of diff models ----
#simple linear regression just to see what we're working with
summary(lm(kickReturnYardage ~ crosses + side_of_field + Id, data = all_returns))
#mixed model with crosses as the random effect
fit_mixed = lmer(kickReturnYardage ~ crosses + side_of_field + (crosses | Id), data = all_returns)
summary(fit_mixed)
ranef(fit_mixed)$Id %>% head(5)
coef(fit_mixed)$Id %>% head(5)
