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

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
q = get_gamelog_data()

Encoding(q$Team) <- "UTF-8"
q$Team = iconv(q$Team, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
team_abbrevs = c(unique(q$Team))


years = c("2019","2020")
ui = shinyUI(
  pageWithSidebar(
    headerPanel("CFB Shit")
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
    
    output$mytable = DT::renderDataTable({   
       df = subset(df, df$Team == input$Teams)
       df = subset(df, df$year == input$Year)

      j = df %>% select(athlete, sum_targets, Team, Opponent, Team_score, Opponent_score, week)
      j = j %>% arrange(sum_targets)
      
      df_data = j %>%
        spread(athlete, sum_targets)
      df_data = df_data %>% arrange(week)
      
      holder = df_data %>% select(Team, Opponent, Team_score, Opponent_score, week)
      excluded_vars = c("Team","Opponent","Team_score","Opponent_score","week")
      
      holder2 = select(df_data, -one_of(excluded_vars))
      
      holder2 <- holder2[,names(sort(colSums(holder2, na.rm = TRUE), decreasing = TRUE))]
      
      df_data = cbind(holder, holder2)
      joinerino = df_data
      
      
      datatable(joinerino)
      
     
      
      datatable(df_data, selection = "single",class = 'cell-border stripe',
                options=list( autoWidth = TRUE, rownames = FALSE,
                              columnDefs = list(list(visible=FALSE)),
                              className = 'dt-center', targets = "_all"))
      
    })
  })


shinyApp(ui = ui, server = server)















