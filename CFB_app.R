### function to put stats online

### add passing attempts and rushing attempts????????????????
### big brain SZN


library("shiny")   
library(DT)
library(googlesheets4)
library(shinycssloaders)
library(dplyr)
library(rdrop2)
library(readr)
library(vroom)
library(tidyverse)


get_gamelog_data = function()
{
  df = vroom::vroom("http://raw.githubusercontent.com/eric-thiel/CFB_Matrix/master/receiving_results.csv.gz")
  return(df)
}

get_longest_reception = function()
{
  df = vroom::vroom("http://raw.githubusercontent.com/eric-thiel/CFB_Matrix/master/longest_reception_summary.csv.gz")
  return(df)
}

get_hv_rush = function()
{
  df = vroom::vroom("http://raw.githubusercontent.com/eric-thiel/CFB_Matrix/master/high_val_rush_summary.csv.gz")
  return(df)
}



header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
q = get_gamelog_data()

Encoding(q$Team) <- "UTF-8"
q$Team = iconv(q$Team, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
team_abbrevs = c(unique(q$Team))


years = c("2021","2020","2019")

stats_to_choose = c("Targets","Passes Thrown","Runs","HV Rush","Ceilings")


ui = shinyUI(
  pageWithSidebar(
    headerPanel("College Football Stats By Game")
    ,
    sidebarPanel(width=2,
                 wellPanel(
                   selectInput("Teams", label = h3("Team Select"),
                                choices =(team_abbrevs), 
                               hr(), ),
                 ),
                 wellPanel(
                   selectInput("Year",label = h3("Year Select"),
                               choices = (years),
                               hr(),)
                 ),
                 wellPanel(
                   selectInput("Stat",label = h3("Stat Select"),
                               choices = (stats_to_choose),
                               hr(),)
                 )
                 
    )
    ,
    
    mainPanel(
      DT::dataTableOutput("mytable"),
    )
    
  ))


server = shinyServer(
  function(input,output,session){
    
    df = get_gamelog_data()
    gf = get_longest_reception()
    tf = get_hv_rush()
    
    output$mytable = DT::renderDataTable({   
      if(input$Stat == "Ceilings"){
        gf = subset(gf, gf$team == input$Teams)
        gf = subset(gf, gf$year == input$Year)
        gf = gf %>% select(targeted, mean_reception, total_targs, games, targs_per_game)
        gf = gf %>% arrange(-total_targs)
        gf$mean_reception = round(gf$mean_reception, 1)
        gf = gf %>% rename("Avg Reception Length"="mean_reception")
        datatable(gf, selection = "single",class = 'cell-border stripe',
                  options=list(autoWidth = TRUE, rownames = FALSE, pageLength = 25,
                               columnDefs = list(list(visible=FALSE)),
                               className = 'dt-center', targets = "_all"))
        
      } else if(input$Stat == "HV Rush") {
        tf = tf %>% filter(offense == input$Teams, year == input$Year)%>%
          select(-year)%>%arrange(-highval_rush)%>%rename("hv / game"="high_val_rushes_per_game")
        datatable(tf, selection = "single",class = 'cell-border stripe',
                  options=list(autoWidth = TRUE, rownames = FALSE, pageLength = 25,
                               columnDefs = list(list(visible=FALSE)),
                               className = 'dt-center', targets = "_all"))
        
      } else {
       df = subset(df, df$Team == input$Teams)
       df = subset(df, df$year == input$Year)
       df = subset(df, df$choice == input$Stat)
      
       
       
      j = df %>% select(athlete, attempts, Team, Opponent, Team_score, Opponent_score, week)
      j = j %>% arrange(attempts)
      
      df_data = j %>%
        spread(athlete, attempts)
      
      df_data = df_data %>% arrange(week)
      
      holder = df_data %>% dplyr::select(Team, Opponent, Team_score, Opponent_score, week)
      excluded_vars = c("Team","Opponent","Team_score","Opponent_score","week")
      
      holder2 = select(df_data, -one_of(excluded_vars))
      
      holder2 <- holder2[,names(sort(colSums(holder2, na.rm = TRUE), decreasing = TRUE))]
      
      df_data = cbind(holder, holder2)
      joinerino = df_data
      
      
      datatable(joinerino)
      
     
      
      datatable(df_data, selection = "single",class = 'cell-border stripe',
                options=list(autoWidth = TRUE, rownames = FALSE, pageLength = 25,
                              columnDefs = list(list(visible=FALSE)),
                              className = 'dt-center', targets = "_all"))
      }
    })
  })


shinyApp(ui = ui, server = server)















