tracking_data_used <- tracking_data_used %>%
  dplyr::mutate(dir_rad = dir * pi / 180,
                v_x = sin(dir_rad) * s,
                v_y = cos(dir_rad) * s,
                v_theta = atan(v_y / v_x),
                v_theta = ifelse(is.nan(v_theta), 0, v_theta))
df_track <- tracking_data_used %>%
  dplyr::filter(gameId == dfplays$gameId, playId == dfplays$playId)

df_track
#trying to find max after a return
football_spots = df_track %>%
  filter(displayName == "football")%>%
  select(gameId, playId, frameId, x, y)%>%
  rename(football.x = x, football.y = y)


df_track
df_track = df_track %>% 
  left_join(football_spots, by=c("gameId", "playId", "frameId"))


dist_to_ball <- function(x, y, football.x, football.y) {
  distance <- sqrt((x-football.x)^2 + (y-football.y)^2)
  return(distance)
}
df_track$dist_to_ball = dist_to_ball(df_track$x, df_track$y,df_track$football.x,df_track$football.y)

influence_radius <- function(dist_to_ball) {
  radius <- 4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ^ 3) / 560 * (dist_to_ball < 15)
  return(radius)
}
df_track$influence_radius = influence_radius(df_track$dist_to_ball)
df_track$sRatio = df_track$s / 13 ^ 2

#matrix tomfoolery
nrow(df_track)
for( i in 1:nrow(df_track)){
  player_coords = matrix(c(df_track$x[i], df_track$y[i]),nrow=1)
  
  s_matrix = matrix(c((df_track$influence_radius[i]*(1 + df_track$sRatio[i])), 0,
                      0, (df_track$influence_radius[i]*(1 - df_track$sRatio[i]))),nrow=2)
  r_matrix = matrix(c(cos(df_track$dir_rad[i]), -sin(df_track$dir_rad[i]), 
                      sin(df_track$dir_rad[i]),cos(df_track$dir_rad[i])),nrow=2)
  
  COV_matrix = ((r_matrix %*% s_matrix) %*% s_matrix) %*% solve(r_matrix)
  norm_fact  = (1/2 * pi) * 1/sqrt(det(COV_matrix))
  mu_play = player_coords + df_track$s[i] * matrix(c(cos(df_track$dir_rad[i]), sin(df_track$dir_rad[i])),nrow=1) / 2
  
  intermed_scalar_player = (player_coords - mu_play) %*% solve(COV_matrix) %*% t(player_coords - mu_play)
  player_influence = norm_fact * exp((-0.5 * intermed_scalar_player))
  df_track$player_influence[i] = player_influence
}


all_returns$frameId_after_1_second = all_returns$frameId.x + 10
all_returns_after_1_second = all_returns %>%
  left_join(tracking_data_used, by=c("gameId", "playId","nflId","frameId" ))%>%
  dplyr::select(-c(time, event.y.y, displayName.y, jerseyNumber, position, team))
View(frame)


test = all_returns %>%
  left_join(tracking_data_used, by=c("gameId", "playId","nflId","frameId" ))
