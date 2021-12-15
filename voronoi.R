#install.packages("ggvoronoi") ----
library(ggvoronoi)
# ----
df_track_voronoi = df_track %>%  
  dplyr::filter(event == "kick_received")
  
df_track_voronoi %>% 
  ggplot(aes(x = x, y = y, fill = team)) + 
  stat_voronoi(geom="path") +
  geom_point(pch = 21, size = 4) + 
  geom_point(data = filter(df_track_voronoi, displayName == "football"), 
             size = 1.5, pch = 21,
             fill = "black")+
  geom_segment(data = df_track_voronoi %>% dplyr::filter(team == "CHI"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
               colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_segment(data = df_track_voronoi %>% dplyr::filter(team == "NO"),
               mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
               colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc")))
