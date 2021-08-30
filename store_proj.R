## backtesting
library(googlesheets4)
gs4_auth(email = "eric.thiel96@gmail.com")
Sals = read_sheet("https://docs.google.com/spreadsheets/d/1p44L6NftCep3x83FCKXVns6oh4l__kgPnZqXQj5i2w0/edit#gid=0",
                       "Sals")


new_stuff = Sals
library(readr)
historic_proj <- read_csv("my_historic_proj.csv.gz")

historic_proj = rbind(historic_proj, new_stuff)


write.csv(historic_proj, file = gzfile("my_historic_proj.csv.gz"), row.names = FALSE)

