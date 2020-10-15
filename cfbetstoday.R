library(googlesheets4)

gs4_auth(email = "eric.thiel96@gmail.com")
cfbets_Sheet1 = read_sheet("https://docs.google.com/spreadsheets/d/117mR0UyE8VscLT6y7OkmcOkN8X_2TyYM-CKGfX_8pqM/edit#gid=0",
                   "Sheet1")
ggs = predict(model, cfbets_Sheet1, type = "prob")



gs4_auth(email = "eric.thiel96@gmail.com")
cfbets_team_score = read_sheet("https://docs.google.com/spreadsheets/d/117mR0UyE8VscLT6y7OkmcOkN8X_2TyYM-CKGfX_8pqM/edit#gid=0",
                           "to_score")

check = predict(model_home_td, cfbets_team_score, type = "prob")
cfbets_team_score = cbind(cfbets_team_score, check)
cfbets_team_score = cfbets_team_score %>% rename("home_td"="1")
cfbets_team_score$`0` = NULL

check = predict(model_home_fg, cfbets_team_score, type = "prob")
cfbets_team_score = cbind(cfbets_team_score, check)
cfbets_team_score = cfbets_team_score %>% rename("home_fg"="1")
cfbets_team_score$`0` = NULL

check = predict(model_away_td, cfbets_team_score, type = "prob")
cfbets_team_score = cbind(cfbets_team_score, check)
cfbets_team_score = cfbets_team_score %>% rename("away_td"="1")
cfbets_team_score$`0` = NULL

check = predict(model_away_fg, cfbets_team_score, type = "prob")
cfbets_team_score = cbind(cfbets_team_score, check)
cfbets_team_score = cfbets_team_score %>% rename("away_fg"="1")
cfbets_team_score$`0` = NULL

cfbets_team_score$sum = cfbets_team_score$home_td + cfbets_team_score$home_fg + cfbets_team_score$away_td + cfbets_team_score$away_fg
cfbets_team_score$sum = 0.98/cfbets_team_score$sum 
cfbets_team_score$home_td = cfbets_team_score$home_td * cfbets_team_score$sum
cfbets_team_score$home_fg = cfbets_team_score$home_fg * cfbets_team_score$sum
cfbets_team_score$away_td = cfbets_team_score$away_td * cfbets_team_score$sum
cfbets_team_score$away_fg = cfbets_team_score$away_fg * cfbets_team_score$sum
cfbets_team_score$sum = cfbets_team_score$home_td + cfbets_team_score$home_fg + cfbets_team_score$away_td + cfbets_team_score$away_fg


cfbets_team_score$bet = ifelse(cfbets_team_score$home_td > (1/cfbets_team_score$home_td_price), "home td", ifelse(
  cfbets_team_score$away_td > (1/cfbets_team_score$away_td_price), "away td",ifelse(
    cfbets_team_score$home_fg > (1/cfbets_team_score$home_fg_price), "home fg",ifelse(
      cfbets_team_score$away_fg > (1/cfbets_team_score$away_fg_price), "away fg","fuck you"
    )
  )
))

cfbets_team_score$kelly = ifelse(cfbets_team_score$bet == "home td",
                             (((cfbets_team_score$home_td_price-1)*cfbets_team_score$home_td) - (1-cfbets_team_score$home_td)) / (cfbets_team_score$home_td_price - 1)
                             ,ifelse(
                               cfbets_team_score$bet == "away td",
                               (((cfbets_team_score$away_td_price-1)*cfbets_team_score$away_td) - (1-cfbets_team_score$away_td)) / (cfbets_team_score$away_td_price - 1)
                               ,ifelse(
                                 cfbets_team_score$bet == "home fg",
                                 (((cfbets_team_score$home_fg_price-1)*cfbets_team_score$home_fg) - (1-cfbets_team_score$home_fg)) / (cfbets_team_score$home_fg_price - 1)
                                 ,ifelse(
                                   cfbets_team_score$bet == "away fg",
                                   (((cfbets_team_score$away_fg_price-1)*cfbets_team_score$away_fg) - (1-cfbets_team_score$away_fg)) / (cfbets_team_score$away_fg_price - 1)
                                   ,0
                               ))))

cfbets_team_score$kelly = cfbets_team_score$kelly *0.25*3000
cfbets_team_score$kelly = round(cfbets_team_score$kelly,0)
cfbets_team_score$kelly = ifelse(cfbets_team_score$kelly <0, 0, cfbets_team_score$kelly)




cfbets_Sheet1 = cbind(cfbets_Sheet1, ggs)
cfbets_Sheet1 = cfbets_Sheet1 %>% rename("prob_fg" = "0", "prob_td"="1")

cfbets_Sheet1$bet = ifelse(cfbets_Sheet1$prob_fg > (1/cfbets_Sheet1$fg), "field goal", ifelse(
  cfbets_Sheet1$prob_td > (1/cfbets_Sheet1$td), "touchdown","nobet"
))

cfbets_Sheet1$kelly = ifelse(cfbets_Sheet1$bet == "field goal",
                             (((cfbets_Sheet1$fg-1)*cfbets_Sheet1$prob_fg) - (1-cfbets_Sheet1$prob_fg)) / (cfbets_Sheet1$fg - 1)
,ifelse(
  cfbets_Sheet1$bet == "touchdown",
  (((cfbets_Sheet1$td-1)*cfbets_Sheet1$prob_td) - (1-cfbets_Sheet1$prob_td)) / (cfbets_Sheet1$td - 1)
,0))

cfbets_Sheet1$kelly = cfbets_Sheet1$kelly *0.25*1000
cfbets_Sheet1$kelly = round(cfbets_Sheet1$kelly,0)

write.csv(cfbets_Sheet1, file ="betstoday1.csv")

write.csv(cfbets_team_score, file = "betstoday2.csv")





