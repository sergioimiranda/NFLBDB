team_ <- "CHI"
returner <- "C.Patterson"

dfplays = KickOffData %>% 
  dplyr::filter(stringr::str_detect(playDescription, returner)) %>%
  dplyr::arrange(-kickReturnYardage) %>%
  dplyr::select(gameId, playId, possessionTeam, playDescription, absoluteYardlineNumber)%>%
  dplyr::slice(2)

df_track <- tracking %>%
  dplyr::filter(gameId == dfplays$gameId, playId == dfplays$playId)

frame = df_track %>%
  filter(event == "kick_received")
#### Distance to ball function ----
footballspotx = frame %>%
  filter(displayName == "football") %>%
  select(x)
footballspoty = frame %>%
  filter(displayName == "football") %>%
  select(y)

dist_to_ball <- function(x, y) {
  distance <- sqrt((x-as.numeric(footballspotx))^2 + (y-as.numeric(footballspoty))^2)
  return(distance)
}
frame$dist_to_ball = dist_to_ball(frame$x, frame$y)

#### influence radius ----
influence_radius <- function(dist_to_ball) {
  radius <- 4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ** 3) / 560 * (dist_to_ball < 15)
  return(radius)
}
frame$influence_radius = influence_radius(frame$dist_to_ball)
frame$sRatio = frame$s / 13 * 2

#matrix tomfoolery
player_coords = matrix(c(frame$x[1], frame$y[1]),nrow=1)

s_matrix = matrix(c((frame$influence_radius[1]*(1 + frame$sRatio[1])), 0,
                    0, (frame$influence_radius[1]*(1 - frame$sRatio[1]))),nrow=2)
r_matrix = matrix(c(cos(frame$v_theta[1]), -sin(frame$v_theta[1]), 
                    sin(frame$v_theta[1]),cos(frame$v_theta[1])),nrow=2)

COV_matrix = matrix(c(sum(sum(sum(r_matrix * s_matrix) * s_matrix))*solve(r_matrix)),nrow=2)

norm_fact  = (1/2 * pi) * 1/sqrt(det(COV_matrix ))
mu_play = player_coords + frame$s[1] * matrix(c(cos(frame$v_theta[1]), sin(frame$v_theta[1])),nrow=1) / 2



