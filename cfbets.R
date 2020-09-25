### derivatives





library(readr)
library(dplyr)
library(nnet)
library(xgboost)
library(tidyverse)
library(readr)
library(dplyr)
library(nnet)
library(xgboost)
library(tidyverse)

library(rstan)
library(lme4)
library(tidyverse)
library(vip)
library(tidymodels)
library(workflows)
library(dials)
library(tune)
library(DT)
library(arm)
library(tidybayes)
library(ggrepel)
library(doParallel)


options(na.action='na.pass')

setwd("~/Documents/Betting_odds")

data <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

betting_odds = data

setwd("~/Documents/CFB_pbp")

data <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

pbp = data

setwd("~/")


play_list <- read_csv("Downloads/College Football Hub - play_types.csv")


pbp = left_join(pbp, play_list[c("play_result_abbrev","play_process_abbrev","text")], by = c("play_type"="text"))
pbp$time_remaining = (4-pbp$period) *15 + pbp$clock.minutes + (pbp$clock.seconds /60)
pbp$score_diff_offence = pbp$offense_score - pbp$defense_score

game_info_2019 <- read_csv("Downloads/game_info_2019.csv")
game_info_2018 <- read_csv("Downloads/game_info_2018.csv")
game_info_2017 <- read_csv("Downloads/game_info_2017.csv")
game_info = rbind(game_info_2018, game_info_2019)
game_info = rbind(game_info, game_info_2017)
rm(game_info_2018)
rm(game_info_2019)
rm(game_info_2017)

pbp$game_id_match = substr(pbp$id, start = 1, stop = 9)
pbp$game_id_match = as.numeric(pbp$game_id_match)

pbp = left_join(pbp, game_info[c("id","season","week")], by = c("game_id_match"="id"))

pbp = pbp %>% arrange(drive_id, -time_remaining)



pbp$first_score = ifelse(lag(pbp$offense_score)== 0 & pbp$offense_score != 0 & pbp$defense_score == 0 & lag(pbp$score_diff_offence) == 0, pbp$offense_score, NA)



sub = subset(pbp, !is.na(pbp$first_score))

first_score = sub %>% group_by(game_id_match)%>%
  summarise(first_score = first(first_score), team_scored = first(offense))



betting_odds = betting_odds %>% group_by(id)%>%
  summarise(home_team = first(homeTeam), away_team = first(awayTeam), spread = first(spread), overunder = first(overUnder))


first_score = left_join(first_score, betting_odds, by  =c("game_id_match"="id"))




first_score = subset(first_score, !is.na(home_team))
first_score = subset(first_score, !is.na(away_team))
first_score = subset(first_score, !is.na(spread))
first_score = subset(first_score, !is.na(overunder))

first_score$first_score = as.numeric(first_score$first_score)

first_score$first_score = ifelse(first_score$first_score == 6 | first_score$first_score == 8, 7, first_score$first_score)







post16 = first_score %>% dplyr::select(first_score, spread, overunder)


post16$is_td = ifelse(post16$first_score == 7, 1, 0)
post16$first_score = NULL
post16$is_td = as.factor(post16$is_td)
library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train(is_td ~ .,
               data = post16,
               trControl = train_control,
               method = "glm",
               family=binomial())

summary(model$results)

asdf = post16
asdf$pred = predict(model, asdf, type = "prob")


cfbets_Sheet1 <- read_csv("Downloads/cfbets - Sheet1.csv")

ggs = predict(model, cfbets_Sheet1, type = "prob")
cfbets_Sheet1 = cbind(cfbets_Sheet1, ggs)
cfbets_Sheet1 = cfbets_Sheet1 %>% rename("prob_fg" = "0", "prob_td"="1")

cfbets_Sheet1$bet = ifelse(cfbets_Sheet1$prob_fg > (1/cfbets_Sheet1$fg), "field goal", ifelse(
  cfbets_Sheet1$prob_td > (1/cfbets_Sheet1$td), "touchdown","nobet"
))











