### FCS PARSING ###

library(readr)
FCS_pbp <- read_csv("~/Downloads/FCS - pbp.csv")

temp = FCS_pbp %>% mutate(
  run = grepl(" rush for | sacked for loss of ", X5), pass = grepl(" pass complete | pass incomplete | pass intercepted", X5), 
  touchdown = grepl("TOUCHDOWN", X5), int = grepl("intercepted", X5), fumbled = grepl("fumble", X5),
  possession_team = ifelse(gsub('.{9}$', '', pbp)==home | gsub('.{9}$', '', pbp)==away, gsub('.{9}$', '', pbp), NA),
  X5 = ifelse(grepl(" loss of ",X5), gsub("loss of ", "-", X5), X5),
  X5 = ifelse(grepl(" sacked for ",X5),gsub("sacked for ", "rush for", X5), X5),
  yardage_gained = ifelse(grepl("NO PLAY|no gain", X5), 0, gsub(".*for (.+) yard.*", "\\1", X5)),
  yardage_gained = ifelse(grepl("to the 50",X5), substr(yardage_gained, start = 1, stop = 2), yardage_gained),
  yardage_gained = ifelse(grepl(" pass incomplete |intercepted", X5),0,yardage_gained),
  yardage_gained = ifelse(grepl("PENALTY", yardage_gained), substr(yardage_gained, start = 1, stop = 2), yardage_gained),
  yardage_gained = as.numeric(yardage_gained),
  rush_player = ifelse(run == TRUE, sub(" rush for.*", "", X5),NA),
  rush_player = ifelse(grepl("NO PLAY", X5), NA, rush_player),
  pass_player = ifelse(pass == TRUE, sub(" pass .*", "", X5), NA),
  pass_player = ifelse(grepl("NO PLAY", X5), NA, pass_player),
  targeted_player = ifelse(pass == TRUE, gsub(".*complete to (.+) for.*", "\\1", X5), NA),
  targeted_player = ifelse(grepl("pass incomplete", X5), sub(".*pass incomplete to ", "", X5), targeted_player),
  targeted_player = ifelse(grepl("dropped pass",X5), sub(", dropped pass.*", "", targeted_player), targeted_player),
  targeted_player = ifelse(grepl(" (", fixed = TRUE, targeted_player), gsub("\\s*\\([^\\)]+\\)","", targeted_player), targeted_player),
  targeted_player = ifelse(grepl("intercepted", X5), NA, targeted_player),
  targeted_player = ifelse(grepl("NO PLAY", X5),NA, targeted_player),
  targeted_player = ifelse(grepl("QB hurry",targeted_player), sub(", QB hurry.*", "", targeted_player) , targeted_player),
  targeted_player = ifelse(grepl("pass incomplete",targeted_player), NA, targeted_player),
  targeted_player = ifelse(grepl(" for ",targeted_player), sub(" for .*", "", targeted_player), targeted_player),
  targeted_player =  gsub('\\.', '', targeted_player),
  receptions = ifelse(grepl("pass complete", X5), 1,0),
  rush_player =  gsub('\\.', '', rush_player),
  pass_player =  gsub('\\.', '', pass_player),
  rush_td = ifelse(run == TRUE & touchdown == TRUE, 1, 0),
  pass_td = ifelse(pass == TRUE & touchdown == TRUE, 1, 0),
  receive_td = ifelse(pass == TRUE & touchdown == TRUE, 1, 0),
  interception = ifelse(pass == TRUE & int == TRUE, 1, 0)
  
  
) %>% fill(possession_team)


## remove game id 10, strange charting, Against FLA ST anyways so its fine.
temp = subset(temp, game_id !=10)

summary_rush = temp %>% filter(run == TRUE) %>% group_by(game_id, rush_player, possession_team)%>%
  summarise(rush_yds = sum(yardage_gained, na.rm = TRUE), tds = sum(rush_td, na.rm = TRUE), attempts = n())

summary_pass = temp %>% filter(pass == TRUE) %>% group_by(game_id, pass_player, possession_team)%>%
  summarise(pass_yds = sum(yardage_gained, na.rm = TRUE), tds = sum(pass_td, na.rm = TRUE), attempts = n())

summary_receive_1 = temp %>% filter(pass == TRUE) %>% group_by(game_id, targeted_player, possession_team)%>%
  summarise(receive_yds = sum(yardage_gained, na.rm = TRUE), tds = sum(pass_td, na.rm = TRUE), attempts = n(),
            receptions = sum(receptions, na.rm = TRUE))

summary_rush = subset(summary_rush, !is.na(rush_player))
summary_pass = subset(summary_pass, !is.na(pass_player))
summary_receive_1 = subset(summary_receive_1, !is.na(targeted_player))

summary_rush = summary_rush %>% mutate(
  fpts = rush_yds*0.1 + tds*6+ifelse(rush_yds >100,3,0),
  choice = "Runs",
  fpts_fd = rush_yds*0.1 + tds*6)%>%rename("athlete" = "rush_player", "Team"= "possession_team") %>% select(athlete, fpts, fpts_fd, attempts, choice, Team)
  
summary_pass = summary_pass %>% mutate(
  fpts = pass_yds*0.04 + tds*4+ifelse(pass_yds >300,3,0),
  choice = "Passes Thrown",
  fpts_fd = pass_yds*0.04 + tds*4)%>%rename("athlete" = "pass_player", "Team"= "possession_team") %>% select(athlete, fpts, fpts_fd, attempts, choice, Team)

summary_receive = summary_receive_1 %>% mutate(
  fpts = receive_yds*0.1 + tds*6+ifelse(receive_yds >100,3,0) + receptions,
  choice = "Targets",
  fpts_fd = receive_yds*0.1 + tds*6 + receptions*0.5)%>%rename("athlete" = "targeted_player", "Team"= "possession_team") %>% select(athlete, fpts, fpts_fd, attempts, choice, Team)

library(readr)
FCS_games <- read_csv("~/Downloads/FCS - games.csv")

together = rbind(summary_rush, summary_pass)
together = rbind(together, summary_receive)

together = left_join(together, FCS_games, by = c("game_id"="game_id"))
together = together %>% mutate(
  Opponent = ifelse(Team == home, away, home),
  Team_score = ifelse(Team == home, home_score, away_score),
  Opponent_score = ifelse(Team ==home, away_score, home_score),
  Team = paste(Team, "FCS", sep = " "),
  Opponent = paste(Opponent, "FCS", sep = " ")
  )%>%
  select(-home, -away, -home_score, -away_score)


summary_longest_fcs = summary_receive_1 %>% group_by(targeted_player)%>%
  summarise(team = paste(first(possession_team), "FCS", sep =" "), mean_reception = (sum(receive_yds, na.rm = TRUE) / sum(receptions, na.rm = TRUE)),
            total_targs = sum(attempts), games = n(), targs_per_game = round((sum(attempts, na.rm = TRUE) / n()),1)
            ) %>% rename("targeted"="targeted_player")



