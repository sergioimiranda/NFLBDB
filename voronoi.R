#Load packages ----
#install.packages("ggvoronoi")
#install.packages("ggforce")
#library(ggvoronoi)
# ----
field_height <- 160/3
field_width <- 120
2018090903

2956
game_voronoi <- "2018090903"
play_voronoi <- "2956"

dfplays_voronoi = KickOffData %>% 
  dplyr::filter(gameId == game_voronoi) %>% 
  dplyr::filter(playId == play_voronoi)  

game_voronoi <- games %>%
  dplyr::filter(gameId == dfplays_voronoi$gameId)

df_track_voronoi <- tracking_data_used_after_3 %>%
  dplyr::filter(gameId == dfplays_voronoi$gameId, playId == dfplays_voronoi$playId)


df_track_voronoi = all_returns %>%
  select(gameId, playId, frameId, keepId) %>%
  right_join(df_track_voronoi, by = c("gameId", "playId", "frameId"))

df_track_voronoi_minusF = df_track_voronoi %>%
  dplyr::filter(!displayName == "football")
#voronoi
df_track_voronoi_minusF %>% 
  ggplot(aes(x = x, y = y, fill = team)) + 
  stat_voronoi(geom="path")+
  geom_point(pch = 21, size = 4) + 
  geom_point(data = filter(df_track_voronoi, displayName == "football"), 
             size = 1.5, pch = 21,
             fill = "black")+
  geom_segment(data = df_track_voronoi_minusF %>% dplyr::filter(team == "MIA"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y), 
               color = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_segment(data = df_track_voronoi_minusF %>% dplyr::filter(team == "TEN"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
               color = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc")))
#density
df_track_voronoi_minusF %>% 
  ggplot(aes(x = x, y = y, z = player_influence)) +
  geom_density_2d_filled(alpha = .75,show.legend=TRUE)+
  scale_fill_brewer(palette = "RdYlBu", direction=-1)+
  geom_point(pch = 19, size = 5,aes(color = team)) + 
  labs(color = "Team") +
  scale_color_manual(values=c("green", "white"))+
  geom_point(data = filter(df_track_voronoi, displayName == "football"), 
             size = 1.5, pch = 21,
             fill = "black") +
  labs(title = dfplays_voronoi$playDescription)+
  # major lines
  annotate("segment", x = c(0, 0, 0,field_width, seq(10, 110, by=5)), xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
    y = c(0, field_height, 0, 0, rep(0, 21)), yend = c(0, field_height, field_height, field_height, rep(field_height, 21))) +
  # hashmarks
  annotate("segment", x = rep(seq(10, 110, by=1), 4),xend = rep(seq(10, 110, by=1), 4),
    y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)), yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101))) +
  # yard numbers
  annotate("text", x = seq(20, 100, by = 10), y = rep(12, 9),
    label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))), size = 5) +
  # yard numbers upside down
  annotate("text", x = seq(20, 100, by = 10), y = rep(field_height-12, 9), 
    label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))), angle = 180, size = 5)+
  geom_segment(data = df_track_voronoi_minusF %>% dplyr::filter(team == "MIA"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y), 
               color = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_segment(data = df_track_voronoi_minusF %>% dplyr::filter(team == "TEN"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
               color = df_colors$home_1, size = 1, arrow = arrow(length = unit(0.01, "npc")))+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())






################################
all_returns_sonar = tracking_data_used_after_3 %>%
  filter(IsReturner == "TRUE")%>%
  rename(nflId = nflId.x) %>%
  select(gameId, playId, nflId, frameId, x_std, y_std, dir_std_1, dir_std_2, x_std_end, y_std_end, a)%>%
  left_join(all_returns, by= c("gameId", "playId", "nflId", "frameId"))

all_returns_sonar$angle_start_end = atan((all_returns_sonar$y_std_end - all_returns_sonar$y_std) / (all_returns_sonar$x_std_end - all_returns_sonar$x_std)) * 180 / pi

all_returns_sonar = all_returns_sonar %>%
  select(gameId, playId, nflId, x_std, y_std, dir_std_1, dir_std_2, x_std_end, y_std_end, angle_start_end, side_of_field_received, successful_return)




aggregate(x=all_returns_sonar$y_std,
          by=list(all_returns_sonar$side_of_field_received),
          FUN=mean)



all_returns_sonar$angle_start_end2 = case_when(all_returns_sonar$side_of_field_received == "Right" ~ ((atan((all_returns_sonar$y_std_end - 15.67) / (all_returns_sonar$x_std_end - 3.036523)) * 180 / pi)),
                                               TRUE ~ (atan((all_returns_sonar$y_std_end - 38.85455) / (all_returns_sonar$x_std_end - 4.582331)) * 180 / pi))         
all_returns_sonar$angle_start_end3 = all_returns_sonar$angle_start_end2  +(-180)





### side sonars
round_angle <- 15
all_returns_sonar <- all_returns_sonar %>% 
  mutate(AngleRound = round(angle_start_end3/round_angle)*round_angle)

sonar <- all_returns_sonar %>%
  mutate(N=n()) %>%
  group_by(AngleRound, side_of_field_received)  %>%
  mutate(n_rushes = n(), n_angle=n_rushes/N) %>%
  filter(n_rushes >= 1) %>% 
  ungroup() %>%
  group_by(side_of_field_received) %>% 
  mutate(maxN = max(n_angle),
         AngleNorm = n_angle/maxN) %>%
  ungroup() %>%
  group_by(AngleRound, side_of_field_received, N)%>%
  summarize(AngleNorm = mean(AngleNorm),
            SuccessRate = mean(successful_return)) %>% 
  arrange(side_of_field_received)


ggplot(sonar) +
  geom_bar(aes(x=AngleRound, y = SuccessRate, fill= SuccessRate), stat="identity") +
  scale_x_continuous(breaks=seq(-360, 360, by=90), limits=c(-360, 360)) +
  coord_polar(start=4.7124, direction=1) + ### rotate 4.7124
  scale_fill_viridis("Success %", na.value="#FDE725FF", 
                     limits = c(0.3, 0.6), 
                     breaks = c(0.3, 0.35, 0.40, .45, .45,.5,.55,.60), 
                     labels = scales::percent) +
  labs(x='', y='') +
  theme_void(13)+
  theme(plot.title = element_text(hjust=.3),
        plot.background = element_rect(fill = "transparent",color = NA),
        panel.background = element_rect(fill = "transparent",color = NA), 
        panel.spacing = unit(0, "lines"), 
        plot.margin = margin(0, 0, 0, 0, "cm")) + 
  facet_wrap(~side_of_field_received, nrow = 1) + 
  labs(title = "Sonars by Field Position, Angled Toward Play End Location", subtitle = " ")+
  scale_fill_gradient(low="red", high="blue") 

