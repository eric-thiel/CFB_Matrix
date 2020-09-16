### function to put stats online


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

team_abbrevs = c("ATL","CHI","CON","DAL","IND","LAS","LVA","MIN",
                 "NYL","PHO","SEA","WAS")
ui = shinyUI(
  pageWithSidebar(
    headerPanel("CFB Shit")
    ,
    sidebarPanel(width=2,
                 wellPanel(
                   radioButtons("Teams", label = h3("Team Select"),
                                choices =(team_abbrevs
                                ), 
                                selected = "ATL"),
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
      df = subset(df, df$TeamAbbreviation == input$Teams)
      
      j = df %>% select(Names, started, matchup, description, game_number)
      
      reference_df = j %>%
        spread(Names, started)
      reference_df = reference_df %>% arrange(game_number)
      reference_df$game_number = NULL
      
      j = df %>% select(Names, Minutes, matchup, description, game_number)
      
      df_data = j %>%
        spread(Names, Minutes)
      df_data = df_data %>% arrange(game_number)
      df_data$game_number = NULL
      
      
      joinerino = cbind(df_data, reference_df)
      
      
      datatable(joinerino)
      
      invisible_start = ncol(df_data)+1
      invisible_end = ncol(joinerino)+1-1
      
      visible_start = 1
      visible_end = ncol(df_data)+1-1
      
      datatable(cbind(df_data,reference_df), selection = "single",class = 'cell-border stripe',
                options=list( autoWidth = TRUE, rownames = FALSE,
                              columnDefs = list(list(visible=FALSE, targets=c(invisible_start:invisible_end))),
                              className = 'dt-center', targets = "_all"))%>%
        formatStyle(visible_start:visible_end, valueColumns=invisible_start:invisible_end,
                    color = JS("value < 0 ? 'red' : value > 0 ? 'blue' : 'black'"))
      
    })
  })


shinyApp(ui = ui, server = server)















