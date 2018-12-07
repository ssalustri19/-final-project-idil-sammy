library(shiny)
library(readr)
library(DT)
library(twitteR)
library(dplyr)
library(lubridate)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(reshape2)

politicians<-read_csv('https://raw.githubusercontent.com/ssalustri19/final-project-idil-sammy/master/politicians.csv')

setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

library_conscientiousness <- read_csv('http://wwbp.org/downloads/public_data/C.top100.1to3grams.gender_age_controlled.rmatrix.csv') %>% mutate(trait = "conscientiousness") %>% select(-con)
library_openness <- read_csv('http://wwbp.org/downloads/public_data/O.top100.1to3grams.gender_age_controlled.rmatrix.csv') %>% mutate(trait = "openness") %>% select(-ope)
library_agreeableness <- read_csv('http://wwbp.org/downloads/public_data/A.top100.1to3grams.gender_age_controlled.rmatrix.csv') %>% mutate(trait = "agreeableness") %>% select(-agr)
library_extraversion <- read_csv('http://wwbp.org/downloads/public_data/E.top100.1to3grams.gender_age_controlled.rmatrix.csv') %>% mutate(trait = "extraversion") %>% select(-ext)
library_neuroticism  <- read_csv('http://wwbp.org/downloads/public_data/N.top100.1to3grams.gender_age_controlled.rmatrix.csv') %>% mutate(trait = "neuroticism") %>% select(-neu)

#join the 5 data sets above. All about personality types
library_fivepersonality <- rbind(library_agreeableness,library_conscientiousness,library_extraversion, library_neuroticism, library_openness)
library_fivepersonality <-library_fivepersonality %>%  rename(word = X1)

ui<- fluidPage(
  titlePanel("Analyzing Politicans' Word Usage in Tweets"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "politician",
                  label="Choose a politician",
                  choices= politicians$twitter_handle,
                  selectize = TRUE),
      numericInput(inputId = "numtweets",
                   label="Select number of most recent tweets to analyze. Max=3200",
                   value=100,
                   min=1,
                   max=3199,
                   step=100),
      actionButton(inputId="goButton",
                   label="Run Analysis")
  
    ),
    mainPanel(
      tabsetPanel(tabPanel("Raw Tweets Data Frame", DT::dataTableOutput(outputId = "tweetstable")),
                  tabPanel("Word Frequency Table", DT::dataTableOutput(outputId = "freqtable")),
                  tabPanel("Word Cloud", plotOutput(outputId="cloud")),
                  tabPanel("Personality Analysis Plot", plotOutput(outputId = "personality_plot")),
                  tabPanel("Positivity and Negativity", plotOutput(outputId = "positivity_plot"))
      )
              
    )
  )
)

  
  
  
  


server<- function(input,output){

  
  tweets_from_selected_politician<-eventReactive(input$goButton,
                                                 {userTimeline(input$politician, n = input$numtweets, includeRts = FALSE)%>%
                                                  twListToDF() %>% 
                                                  mutate(date=lubridate::date(created)) %>% 
                                                  select(text, date)
                                                  })
  freq<-reactive({
    raw<-tm::termFreq(tweets_from_selected_politician()$text, control = list(removePunctuation = TRUE, tolower = TRUE, stopwords = TRUE)) 
    df <- as.data.frame(melt(as.matrix(raw), varnames = c("word", "some"))) %>% select(-some)
    df$word <- as.character(df$word)
    df2<- df %>% arrange(desc(value)) 
  })
  
  output$freqtable <- DT::renderDataTable({
    DT::datatable(data = freq(), 
                options = list(pageLength = 10), 
                rownames = FALSE)
  })
    
  output$cloud<-renderPlot({
    set.seed(1234)
    wordcloud::wordcloud(freq()$word, freq()$value, min.freq=5, max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=RColorBrewer::brewer.pal(8, "Dark2"))
  })


  
  output$tweetstable<-DT::renderDataTable({
    DT::datatable(data = tweets_from_selected_politician(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  output$personality_plot<-renderPlot({
    inner_join(library_fivepersonality, freq(), by=c("word"="word")) %>% group_by(trait) %>% summarize(word_count=sum(value)) %>% ggplot(aes(x=trait, y=word_count))+geom_bar(stat="identity")
  })
  output$positivity_plot<-renderPlot({
    tweets_from_selected_politician() %>% mutate(positivity_rating=analyzeSentiment(text)$SentimentQDAP) %>% ggplot(aes(x=positivity_rating))+geom_histogram()
  })
}
  
  


shinyApp(ui=ui, server=server)