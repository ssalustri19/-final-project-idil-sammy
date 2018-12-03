library(shiny)
library(readr)
library(DT)
library(twitteR)
library(dplyr)
library(lubridate)

politicians<-read_csv('https://raw.githubusercontent.com/ssalustri19/final-project-idil-sammy/master/politicians.csv')

setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

ui<- fluidPage(
  titlePanel("Analyzing Politicans' Word Usage in Tweets"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "politician",
                  label="Choose a politician",
                  choices= politicians$twitter_handle,
                  selectize = TRUE)
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "tweetstable")
    )
  )
  
  
  
  
)

server<- function(input,output){
  output$tweetstable<-DT::renderDataTable({
    req(input$politician)
    tweets_from_selected_politician<-userTimeline(input$politician, n=100, includeRts = FALSE)%>%
    twListToDF() %>% select(text, created, screenName)
    DT::datatable(data = tweets_from_selected_politician, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  
}

shinyApp(ui=ui, server=server)