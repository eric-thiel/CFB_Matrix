library(googlesheets4)

gs4_auth(email = "eric.thiel96@gmail.com")
cfbets_Sheet1 = read_sheet("https://docs.google.com/spreadsheets/d/117mR0UyE8VscLT6y7OkmcOkN8X_2TyYM-CKGfX_8pqM/edit#gid=0",
                   "Sheet1")
ggs = predict(model, cfbets_Sheet1, type = "prob")
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

