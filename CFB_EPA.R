#### CFB epa adjusting

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

set.seed(1234)


options(na.action='na.pass')
### hopefully this will work
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

#pbp = subset(pbp, play_process_abbrev == "PASS" | play_process_abbrev == "RUSH")

pbp = subset(pbp, game_id_match > 10)
pbp$triple_option_team = ifelse(pbp$season == 2017 & pbp$offense == "Georgia Tech",1,
                                ifelse(pbp$season == 2018 & pbp$offense == "Georgia Tech",1,
                                       ifelse(pbp$offense == "Georgia Southern",1,
                                              ifelse(pbp$offense == "Army",1,
                                                     ifelse(pbp$offense == "Navy",1,
                                                            ifelse(pbp$offense == "Air Force",1,0))))))

#pbp$is_neutral_sit = ifelse(abs(pbp$offense_score - pbp$defense_score)<10 & pbp$period <4,1,0)

#neutral_sit = subset(pbp, is_neutral_sit == 1)

pbp$is_pass = ifelse(pbp$play_process_abbrev == "PASS",1,0)
pbp$is_rush = ifelse(pbp$play_process_abbrev == "RUSH",1,0)
pbp$under_2_first_half = ifelse(pbp$period == 2 & pbp$clock.minutes < 2,1,0)

pbp$half_seconds_remaining = ifelse(pbp$period == 1 | pbp$period == 3, (pbp$clock.minutes *60) + pbp$clock.seconds + (15*60),
                                    ifelse(pbp$period == 2 | pbp$period == 4, pbp$clock.minutes*60 + pbp$clock.seconds, "untimed"))
pbp$half_seconds_remaining = as.numeric(pbp$half_seconds_remaining)

pbp = pbp %>% arrange(game_id_match,-time_remaining)

### epa performance ### 
## make epa based on shit FAK ##

### next score ###

pbp$half = ifelse(pbp$period <3, 1, 2)

okauy = pbp %>% group_by(game_id_match, half)%>%
  mutate(end_half = last(half_seconds_remaining))%>%ungroup()

okauy$end_half = ifelse(okauy$half_seconds_remaining == okauy$end_half, 1, 0)


