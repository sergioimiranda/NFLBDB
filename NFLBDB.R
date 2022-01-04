#load packages ----
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
players$nflId = as.factor(players$nflId) #Ids as factors instead of characters for modeling use
plays = read.csv("plays.csv")
plays = rename(plays, nflId = returnerId) #Renaming returnerId to nflId to match other data frames
plays$nflId = as.factor(plays$nflId) #Ids as factors instead of characters for modeling use

#Merging Scout data & play data ----
DataMerge = merge(plays,PFFScout, by= c("gameId","playId"), all =TRUE) #combining play data and PFF scout data to a main data frame

KickOffData = DataMerge %>%
  filter(specialTeamsPlayType == "Kickoff") %>% #returning only kickoffs
  filter(specialTeamsResult == "Return") #returning only plays that have a return (keep in mind there are some touchbacks that are labled as returned)

toMatch = c("onside", "MUFFS", "FUMBLES", "Lateral","Touchback") #Removing anything that has onside, muff, fumble, lateral, or touchback in the play description
KickOffData =  KickOffData[-grep(paste(toMatch,collapse="|"), KickOffData$playDescription), ]

KickOffData = KickOffData %>% 
  left_join(games, by=c("gameId"))%>% #joining game data for home/away team data
  drop_na(season, week, gameDate, gameTimeEastern)

KickOffData$scoreDifferential = case_when(KickOffData$possessionTeam == KickOffData$homeTeamAbbr ~ KickOffData$preSnapVisitorScore - KickOffData$preSnapHomeScore, 
                                              TRUE ~ KickOffData$preSnapHomeScore - KickOffData$preSnapVisitorScore) #Finding the pre-snap score differential 
KickOffData$scoreDifferential= case_when(KickOffData$scoreDifferential >= 27 ~ 27, 
                                         KickOffData$scoreDifferential <= -27 ~ -27,
                                         TRUE ~ as.numeric(KickOffData$scoreDifferential)) #maxing out score differential to +- 27 points

KickOffData = KickOffData %>%
  select(gameId, playId, playDescription, quarter, possessionTeam, nflId, kickReturnYardage, scoreDifferential, homeTeamAbbr, visitorTeamAbbr, absoluteYardlineNumber)

#Import Tracking Data ----
tracking2018 = read.csv("tracking2018.csv")
tracking2019 = read.csv("tracking2019.csv")
tracking2020 = read.csv("tracking2020.csv")

#Filtering Tracking Data for Kickoffs Only
tracking_data_used_bind = rbind(tracking2018, tracking2019, tracking2020) %>% #using tracking_data_used_bind to save time from having to load each individual tracking_year data
  dplyr::filter(gameId %in% KickOffData$gameId & playId %in% KickOffData$playId) #filter in to only the gameIds or playId in KickoffData

#Finding the min/max y position of the ball during the return
tracking_data_used = tracking_data_used_bind %>%
  group_by(gameId, playId, displayName) %>% 
  mutate(after_catch = cumany(event == "kick_received")) #labeling each frame to whether it was before or after the kickoff was received

max.y = tracking_data_used %>% #finding the max y of each return after the catch
  filter(displayName == "football") %>%
  filter(after_catch == "TRUE")%>%
  group_by(gameId, playId)%>%
  slice(which.max(y)) %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(max.x = x, max.y = y)
  
min.y = tracking_data_used %>% #finding the min y of each return after the catch
  filter(displayName == "football") %>%
  filter(after_catch == "TRUE")%>%
  group_by(gameId, playId)%>%
  slice(which.min(y)) %>%
  select(gameId, playId, x, y, frameId)%>%
  rename(min.x = x, min.y = y)

#Filtering more tracking data ----

tracking_kickoff = tracking_data_used %>% #Filtering tracking data to only kickoff frames
  dplyr::filter(event == "kickoff") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId,team) %>%
  dplyr::rename(x_kickoff = x, y_kickoff = y, event_kickoff = event, displayName_kickoff = displayName, frameId_kickoff = frameId) 

tracking_kick_received = tracking_data_used %>% #Filtering tracking data to only kickoff reception frames
  dplyr::filter(event == "kick_received") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) %>%
  dplyr::rename(x_received = x, y_received = y, event_received = event, displayName_received = displayName, frameId_received = frameId) 

#Filtering tracking data to only tackles, touchdowns, or ran out of bounds, removes ball that bounced out of bounds or recovered by kickoff team 
#(this does remove some legitimate returns, but they aren't label in the tracking data, so it's impossible to know when the play ends)
tracking_play_end = tracking_data_used %>%
  dplyr::filter(event =="tackle" | event =="touchdown" | event =="out_of_bounds") %>%
  dplyr::filter(displayName == "football") %>%
  dplyr::select(gameId, playId, x, y, event, displayName, frameId) %>%
  dplyr::rename(x_play_end = x, y_play_end = y, event_play_end = event, displayName_play_end = displayName, frameId_play_end = frameId)

#Joining tracking data and kick off play data ----
all_returns = tracking_kick_received %>% #joining frames kick received frames, and end of play frames
  left_join(tracking_play_end, by=c("gameId", "playId")) %>%
  left_join(max.y, by=c("gameId", "playId")) %>% #joining max y frames
  left_join(min.y, by=c("gameId", "playId")) %>% #joining min y frames
  dplyr::filter(!between(y_received, 23.3, 29.9)) %>% #removing anything that was caught inside the hashes
  drop_na(y_play_end) #removes plays that don't have a play end marker

all_returns = all_returns %>% #joining tracking data with kickoff data
  left_join(KickOffData, by=c("gameId", "playId"))
all_returns = all_returns %>% 
  drop_na(playDescription) #this is dropping muffs, fumbles, etc.

all_returns$return_direction = case_when(all_returns$x_received < 50 ~ "RIGHT", TRUE ~ "LEFT") #Which direction is the return going

all_returns$side_of_field_received = case_when(all_returns$y_received >= 29.9 ~ "Left", TRUE ~ "Right") #which side of the field was ball received

all_returns$side_of_field_received = case_when((all_returns$x_received >= 50 & all_returns$side_of_field_received == "Right" ~ "Left"), #side, based on return direction
                                      (all_returns$x_received >= 50 & all_returns$side_of_field_received == "Left" ~ "Right"),
                                      (all_returns$x_received <= 50 & all_returns$side_of_field_received == "Left" ~ "Left"),
                                      (all_returns$x_received <= 50 & all_returns$side_of_field_received == "Right" ~ "Right"))

all_returns$successful_return = case_when(all_returns$return_direction == "RIGHT" & all_returns$x_play_end >= 35 ~ TRUE, #labeling if the return was a success by determing if they at least reached the 25 yard line
                                          all_returns$return_direction == "RIGHT" & all_returns$x_play_end < 35 ~ FALSE,
                                          all_returns$return_direction == "LEFT" & all_returns$x_play_end >= 85 ~ TRUE,
                                          all_returns$return_direction == "LEFT" & all_returns$x_play_end < 85 ~ FALSE)

all_returns$crosses = case_when(all_returns$side_of_field_received == "Right" & all_returns$return_direction == "RIGHT" & all_returns$max.y >=26.6 ~ TRUE, #did they cross field T/F
                                all_returns$side_of_field_received == "Right" & all_returns$return_direction == "LEFT" & all_returns$min.y <=26.6 ~ TRUE,
                                all_returns$side_of_field_received == "Left" & all_returns$return_direction == "RIGHT" & all_returns$max.y <=26.6 ~ TRUE,
                                all_returns$side_of_field_received == "Left" & all_returns$return_direction == "LEFT" & all_returns$max.y >=26.6 ~ TRUE,
                                TRUE ~ FALSE)
all_returns$returnTeam = case_when(all_returns$possessionTeam == all_returns$homeTeamAbbr ~ all_returns$visitorTeamAbbr, TRUE ~ all_returns$homeTeamAbbr) #labling which team is returning or kicking

all_returns = all_returns %>% #joining returner player data
  left_join(players, by=c("nflId"))

#creating fillerId to assign to returners that have had less than 5 returns
all_returns = as.data.frame(all_returns) #ungrouping data frame
return_count_by_Id = dplyr::count(all_returns, nflId)
return_count_by_Id$Id = case_when(return_count_by_Id$n <= 5 ~ "filler_Id", TRUE ~ as.character(return_count_by_Id$nflId))
#returning ID class from character back to factor
return_count_by_Id$Id =  as.factor(return_count_by_Id$Id) #returning ID class from character back to factor

all_returns = all_returns %>% 
  left_join(return_count_by_Id, by=c("nflId"))

all_returns$frameId = all_returns$frameId_received + 3 #Labeling the frame .3 seconds after reception

all_returns = all_returns %>%
  filter(!between(x_received,36,84)) #removes plays caught after the 26 yard line

tracking_data_used = all_returns %>% #joining the return data with the rest of the tracking data
  select(gameId,playId,return_direction, successful_return, returnTeam,possessionTeam,homeTeamAbbr,visitorTeamAbbr, nflId)%>%
  left_join(tracking_data_used, by=c("gameId", "playId"))

tracking_data_used <- tracking_data_used %>%
  dplyr::mutate(dir_rad = dir * pi / 180,
                v_x = sin(dir_rad) * s,
                v_y = cos(dir_rad) * s,
                v_theta = atan(v_y / v_x),
                v_theta = ifelse(is.nan(v_theta), 0, v_theta))

tracking_data_used$team_home_or_away = tracking_data_used$team #labeling who is on what team

tracking_data_used$team = case_when(tracking_data_used$team == "home" ~ tracking_data_used$homeTeamAbbr, TRUE ~ tracking_data_used$visitorTeamAbbr) #labeling who is on what team

tracking_data_used <- tracking_data_used %>% #creating left label for standardization of x & y
  mutate(ToLeft = return_direction == "LEFT", 
         IsOnReturnTeam = team == returnTeam,
         IsReturner = nflId.y == nflId.x)

tracking_data_used <- tracking_data_used %>% 
  mutate(x_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
         y_std = ifelse(ToLeft, 160/3-y, y))    ## Standardized Y

tracking_data_used <- tracking_data_used %>% 
  mutate(dir_std_1 = ifelse(ToLeft & dir < 90, dir + 360, dir), 
         dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, dir_std_1), 
         dir_std_2 = ifelse(ToLeft, dir_std_1 - 180, dir_std_1))

tracking_data_used <- tracking_data_used %>% 
  mutate(x_std_end = s*cos((90-dir_std_2)*pi/180) + x_std, 
         y_std_end = s*sin((90-dir_std_2)*pi/180) + y_std)

#Player Influence ----
football_spots = tracking_data_used %>% #labeling where the football is at each frame
  filter(displayName == "football")%>%
  select(gameId, playId, frameId, x_std, y_std)%>%
  rename(football.x = x_std, football.y = y_std)

tracking_data_used = tracking_data_used %>% #joining football location with tracking data
  left_join(football_spots, by=c("gameId", "playId", "frameId"))

dist_to_ball <- function(x_std, y_std, football.x, football.y) { #distance from each player location to the ball function
  distance <- sqrt((x_std-football.x)^2 + (y_std-football.y)^2)
  return(distance)
}

tracking_data_used = tracking_data_used %>%
  mutate(dist_to_ball = dist_to_ball(x_std, y_std, football.x, football.y))

influence_radius <- function(dist_to_ball) {
  radius <- 4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ^ 3) / 560 * (dist_to_ball < 15)
  return(radius)
}

tracking_data_used = tracking_data_used %>%
  mutate(influence_radius = influence_radius(dist_to_ball)) %>%
  mutate(sRatio = s / 13 ^ 2) 

all_returns = all_returns %>% #creating a keep identifier to match with frameId
  mutate(keepId = "KEEP")

all_returns_keep = all_returns %>%
  select(gameId, playId, keepId, frameId)

tracking_data_used_after_3 = tracking_data_used %>% #tracking data frame only for frames .3 seconds after the catch
  left_join(all_returns_keep, by=c("gameId", "playId","frameId"))

tracking_data_used_after_3 = tracking_data_used_after_3 %>%
  filter(keepId == "KEEP")




for( i in 1:nrow(tracking_data_used_after_3)){
  player_coords = matrix(c(tracking_data_used_after_3$x[i], tracking_data_used_after_3$y[i]),nrow=1)
  
  s_matrix = matrix(c((tracking_data_used_after_3$influence_radius[i]*(1 + tracking_data_used_after_3$sRatio[i])), 0,
                      0, (tracking_data_used_after_3$influence_radius[i]*(1 - tracking_data_used_after_3$sRatio[i]))),nrow=2)
  r_matrix = matrix(c(cos(tracking_data_used_after_3$dir_rad[i]), -sin(tracking_data_used_after_3$dir_rad[i]), 
                      sin(tracking_data_used_after_3$dir_rad[i]),cos(tracking_data_used_after_3$dir_rad[i])),nrow=2)
  
  COV_matrix = ((r_matrix %*% s_matrix) %*% s_matrix) %*% solve(r_matrix)
  norm_fact  = (1/2 * pi) * 1/sqrt(det(COV_matrix))
  mu_play = player_coords + tracking_data_used_after_3$s[i] * matrix(c(cos(tracking_data_used_after_3$dir_rad[i]), sin(tracking_data_used_after_3$dir_rad[i])),nrow=1) / 2
  
  intermed_scalar_player = (player_coords - mu_play) %*% solve(COV_matrix) %*% t(player_coords - mu_play)
  player_influence = norm_fact * exp((-0.5 * intermed_scalar_player))
  tracking_data_used_after_3$player_influence[i] = player_influence
}
#distance of each location to the 25
tracking_data_used_after_3$distance_to_25 = case_when(tracking_data_used_after_3$return_direction == "RIGHT" ~ 35-tracking_data_used_after_3$x,
                                                      TRUE ~ tracking_data_used_after_3$x - 85)

#separating kick team and return team
tracking_data_used_after_3_kick_team = tracking_data_used_after_3 %>%
  filter(IsOnReturnTeam == FALSE)
tracking_data_used_after_3_returner = tracking_data_used_after_3 %>%
  filter(IsReturner == TRUE) %>%
  select(gameId, playId, player_influence, distance_to_25, a) #selecing field control of returner, acceleration, and distance to the 25, 3 frames after catch

#max player influence of the kick team 3 frames after the catch (includes, distance to ball)
max_player_influence_kick_team_by_distance = tracking_data_used_after_3_kick_team %>%
  filter(!displayName == "football")%>%
  group_by(gameId, playId) %>%
  slice(which.min(dist_to_ball)) %>%
  select(gameId, playId, player_influence, dist_to_ball)

all_returns = all_returns %>% #joining frame 3 data with return data 
  left_join(max_player_influence_kick_team_by_distance, by = c("gameId", "playId")) %>%
  left_join(tracking_data_used_after_3_returner, by = c("gameId", "playId")) 
#Looking at each return type ----
aggregate(x=all_returns$kickReturnYardage,
          by=list(all_returns$crosses,all_returns$side_of_field_received),
          FUN=mean)

ggplot(all_returns, aes(side_of_field_received, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(crosses, kickReturnYardage))+
  geom_boxplot()

ggplot(all_returns, aes(crosses, side_of_field_received, fill= kickReturnYardage, label = kickReturnYardage)) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="black") +
  geom_text() 

ggplot(all_returns, aes(y=crosses, x=side_of_field_received, color=side_of_field_received, group=successful_return)) +
  geom_count(alpha=0.5) 

#visualize more freq returners
sample_plays <- all_returns %>% 
  group_by(Id) %>%
  filter(n() >= 35)

ggplot(sample_plays,aes(x = crosses, y=kickReturnYardage))+
  geom_boxplot()+ 
  facet_grid(.~Id)

#Significance testing for which side the ball was received on & whether the return crossed field ----
no_cross_return = all_returns %>%
  dplyr::filter(crosses == "FALSE")

cross_returns = all_returns %>%
  dplyr::filter(crosses == "TRUE")  

right_return = all_returns %>%
  dplyr::filter(side_of_field_received == "Right")

left_return = all_returns %>%
  dplyr::filter(side_of_field_received == "Left") 
# t test for difference in crossing
t.test(right_return$kickReturnYardage,left_return$kickReturnYardage)
# there is a difference on the side of field

# t test for side of the field we start on
t.test(no_cross_return$kickReturnYardage,cross_returns$kickReturnYardage)
#there is a difference for cross vs no cross

#couple of diff models ----
#simple linear regression just to see what we're working with
summary(lm(kickReturnYardage ~ crosses + side_of_field_received + scoreDifferential, data = all_returns))
#mixed model with crosses as the random effect
summary(lmer(kickReturnYardage ~ side_of_field_received + scoreDifferential + (crosses | Id), data = all_returns))
#summary(lmer(kickReturnYardage ~ #score differential + acceleration(avg first 3 frames) + FC + FC of nearest opponent + distance from nearest opponent (same as FC) + distance to 25 yard line + side of the field + fixed effect of crossing or not by nflID)
summary(lmer(kickReturnYardage ~ scoreDifferential + a + player_influence.x + player_influence.y + dist_to_ball + distance_to_25 + 
               side_of_field_received + (1 | crosses), data = all_returns))

summary(lm(kickReturnYardage ~ scoreDifferential + a + player_influence.x + player_influence.y + dist_to_ball + distance_to_25 + 
               side_of_field_received + crosses, data = all_returns))

