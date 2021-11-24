#Install packages ----
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("nflfastR")
#load packages
library(tidyverse)
#library(nflfastR)


#Import Data ----
PFFScout = read.csv("PFFScoutingData.csv")
games = read.csv("games.csv")
players = read.csv("players.csv")
players = rename(players, returnerId = nflId)
plays = read.csv("plays.csv")
tracking2018 = read.csv("tracking2018.csv")
tracking2019 = read.csv("tracking2019.csv")
tracking2020 = read.csv("tracking2020.csv")

tracking2018$year = 2018
tracking2019$year = 2019
tracking2020$year = 2020
tracking = rbind(tracking2018, tracking2019, tracking2020)

#Merging Scout data & play data ----
DataMerge = merge(plays,PFFScout, by= c("gameId","playId"), all =TRUE)

#Subsetting kickoff data
KickOffData = subset(DataMerge, specialTeamsPlayType == "Kickoff")

#pull kickoffs that are caught outside of the hashmarks and are returned ----
KickOffData1 = select(KickOffData, gameId, playId, kickReturnYardage, returnerId)

#all plays where there is a kickoff
trackingKickoff = tracking %>%
  dplyr::filter(event == "kickoff") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, event)

#all plays where a ball is caught (includes punts, kickoffs, safeties)
trackingKick_received = tracking %>%
  dplyr::filter(event == "kick_received") %>%
  dplyr::filter(stringr::str_detect(displayName, "football")) %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) 

#plays that ended with a tackle, touchdown, or ran out of bounds
trackingEvent2 = tracking %>%
  dplyr::filter(event =="tackle" | event =="touchdown" | event =="out_of_bounds") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId)

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

all_returns = all_returns %>% 
  drop_na(kickReturnYardage)

no_cross_return = all_returns %>%
  dplyr::filter(crosses == "FALSE")

cross_returns = all_returns %>%
  dplyr::filter(crosses == "TRUE")  

mean(no_cross_return$kickReturnYardage)
mean(cross_returns$kickReturnYardage)

all_returns$returnerId = as.numeric(all_returns$returnerId)
all_returns = all_returns %>%
  drop_na(returnerId)

all_returns$side_off_field = case_when(all_returns$y.x >= 29.9 ~ "Right", TRUE ~ "Left")

all_returns = all_returns %>% 
  left_join(players, by=c("returnerId"))

all_returns$displayName = as.factor(all_returns$displayName)
all_returns$crosses =   as.factor(all_returns$crosses)
all_returns$side_off_field =  as.factor(all_returns$side_off_field)

all_returns = all_returns %>%
  dplyr::select(-c(displayName.y, height, weight, birthDate, collegeName, Position))

### ----
#returner returns count
dplyr::count(all_returns, displayName)

ggplot(all_returns, aes(crosses, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(side_off_field, kickReturnYardage))+
  geom_boxplot()

fit.1 = lm(kickReturnYardage ~ crosses + side_off_field + displayName, data = all_returns)
summary(fit.1)

table(all_returns$displayName, all_returns$crosses)

newdata = data.frame(crosses="FALSE", side_off_field="Right", displayName="Jamal Agnew")
predict(fit.1, newdata)
ggplot(all_returns, aes(crosses, side_off_field, fill= kickReturnYardage)) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="black") 
  