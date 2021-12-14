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
             fill = "black")
