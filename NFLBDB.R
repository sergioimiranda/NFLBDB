#Install packages ----
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("nflfastR")

#load packages
library(tidyverse)
library(nflfastR)

#Import Data ----
PFFScout = read.csv("PFFScoutingData.csv")
games = read.csv("games.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")
tracking2018 = read.csv("tracking2018.csv")
tracking2019 = read.csv("tracking2019.csv")
tracking2020 = read.csv("tracking2020.csv")

#Merging Scout data & play data ----
DataMerge = merge(plays,PFFScout, by= c("gameId","playId"), all =TRUE)

#Subsetting kickoff data
KickOffData = subset(DataMerge, specialTeamsPlayType == "Kickoff")

# remove safety, onsides, and squibs kicks. Basically only keeping normal kicks
KickOffData = subset(KickOffData, !(kickType %in% c("K", "O", "Q", "S")))
KickOffData = KickOffData[-grep("onside", KickOffData$playDescription), ]

#Marker for the spot of the ball at the end of the play
KickOffData$spotOnFieldFinal = 100 - KickOffData$yardlineNumber - KickOffData$playResult


KickOffData$spotFielded1 = word(KickOffData$playDescription,10)
KickOffData$spotFielded1 = str_sub(KickOffData$spotFielded, end = -2)
KickOffData$spotFielded2 = word(KickOffData$playDescription,9)
KickOffData$spotFielded2 = str_sub(KickOffData$spotFielded2, end = -2)

KickOffData$spotFielded = ifelse(KickOffData$yardlineNumber == "50" ,KickOffData$spotFielded2,KickOffData$spotFielded1)
KickOffData$spotFielded = ifelse(KickOffData$specialTeamsResult == "Touchback" ,"25",KickOffData$spotFielded)
KickOffData = KickOffData[!is.na(KickOffData$spotFielded),]
KickOffData$spotFielded = as.numeric(KickOffData$spotFielded)

##gg ----
ggplot(KickOffData, aes(spotFielded, spotOnFieldFinal))+
  geom_point(aes(color = factor(kickType)))

# Creating buckets ----
KickOffData$spotFieldedBucket = "" 
KickOffData$spotFieldedBucket = cut(KickOffData$spotFielded, breaks = c(-20, 0,5,10,15,20,25,30,100))

ggplot(KickOffData, aes(spotFieldedBucket, spotOnFieldFinal))+
  geom_boxplot()

# Expected score based on starting field postion

View(KickOffData)
