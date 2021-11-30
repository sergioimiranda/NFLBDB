install.packages("ggvoronoi")
library(ggvoronoi)
names(df_track)

df_track_kick_received = df_track %>%  
  dplyr::filter(event == "kick_received")
  
  
df_track_kick_received %>% 
  ggplot(aes(x = x, y = y, fill = team)) + 
  stat_voronoi(geom="path") +
  geom_point(pch = 21, size = 4) + 
  geom_point(data = filter(df_track_kick_received, displayName == "football"), 
             size = 1.5, pch = 21,
             fill = "black") 


  scale_colour_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = 0)), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(0:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate", title = "Sample plays, standardized", 
       subtitle = "Offense moving left to right") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank())
