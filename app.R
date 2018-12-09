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
library(SentimentAnalysis)
library(lexicon)
library(DescTools)
library(jmuOutlier)

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

#the thinking type/psychoanalysis library; cleans it up and makes it usable
 primordial_thinking_library<- lexicon::key_regressive_imagery %>% rename(word = regex, type_of_thinking = thinking) %>% mutate(word = gsub("[^0-9A-Za-z///' ]","'" , word ,ignore.case = TRUE)) %>% mutate(word = sub("\\'b$", "", word)) %>% mutate(word = sub("\\'", "", word)) %>% mutate(word = sub("^b", "", word)) %>% select(word, type_of_thinking, category)

   
ui<- fluidPage(
  titlePanel("Analyzing Politicans' Tweets"),
  
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
      tabsetPanel(tabPanel("Raw Tweets Data Frame", 
                           textOutput(outputId = "position_party"), 
                           DT::dataTableOutput(outputId = "tweetstable")),
                  tabPanel("Word Frequency Table", 
                           DT::dataTableOutput(outputId = "freqtable")),
                  tabPanel("Word Cloud", 
                           plotOutput(outputId="cloud")),
                  tabPanel("Positivity and Negativity Analysis", 
                           plotOutput(outputId = "positivity_plot")),
                  tabPanel("Personality Analysis", 
                           plotOutput(outputId = "personality_plot"),
                           h6("This analysis is based on the `World Well-Being Project` by UPENN; http://www.wwbp.org/publications.html"),
                           h6("Schwartz, H.Andrew, et al. “Personality, Gender, and Age in the Language of Social Media: The Open-Vocabulary Approach.” PLoS ONE, vol. 8, no. 9, Sept. 2013, pp. 1–16")),
                  tabPanel("Thought Analysis", 
                           plotOutput(outputId = "primordial_plot"),
                           h4("Conceptual thought is abstract, logical, reality oriented, and aimed at problem solving. Primordial thought is associative, concrete, and takes little account of reality."),
                           DT::dataTableOutput(outputId = "test"), 
                           br(),
                           h6("Our analysis uses the `Key regressive imagery` dictionary from the lexicon package. It is based on the following papers:"), 
                              h6("Martindale, C. (1975). Romantic progression: The psychology of literary history. Washington, D.C.: Hemisphere."),
                              h6("Martindale, C. (1976). Primitive mentality and the relationship between art and society. Scientific Aesthetics, 1, 5218."),
                              h6("Martindale, C. (1977). Syntactic and semantic correlates of verbal tics in Gilles de la Tourette's syndrome: A quantitative case study. Brain and Language, 4, 231-247."),
                              h6("Martindale, C. (1990). The clockwork muse: The predictability of artistic change. New York: Basic Books."))
      )
    )
  )
)


server<- function(input,output){

  output$position_party<-renderText({
    req(input$politician)
    df<-politicians %>% filter(twitter_handle==input$politician) %>% select(name,position,party)
    name<-df$name
    party<-df$party
    position<-df$position
    if (party=="D") party<-"democratic" else if (party=="R") party<-"republican" else party<-"independent"
    paste(name, " is a ", party, position, ".")
  })
  
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
    df <- df %>% filter(!word %like% "http%", !word %like% "^amp%") %>% arrange(desc(value)) 
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
    inner_join(library_fivepersonality, freq(), by=c("word"="word")) %>% group_by(trait) %>% summarize(word_count=sum(value)) %>% ggplot(aes(x=trait, y=word_count))+geom_bar(stat="identity")+xlab("Personality trait associated with each word")+ylab("Number of Words")+ggtitle("Personality Analysis Plot")
  })
  
  output$positivity_plot<-renderPlot({
    tweets_from_selected_politician() %>% mutate(positivity_rating=analyzeSentiment(text)$SentimentQDAP) %>% ggplot(aes(x=positivity_rating, fill = as.factor(sign(positivity_rating))))+geom_histogram(binwidth = .1) + scale_fill_manual(values=c("darkred", "gray", "darkgreen"), name="Positive or Negative?",breaks=c("-1", "0", "1"), labels=c("More Negative","Neutral","More Positive"))+xlab("Net Positivity Rating Per Tweet")+ylab("Number of Tweets")+ggtitle("Positivity of Tweets Distrinbution")
  })
    
  output$primordial_plot<-renderPlot({
    inner_join(primordial_thinking_library, freq(), by = c("word" = "word")) %>% group_by(type_of_thinking) %>% ggplot(aes(x=type_of_thinking, y=value, fill = category))+geom_col()+ggtitle("How does a politician think/communicate, with primordial or conceptual language?")+xlab("Thinking type")+ylab("word frequency")
  })
  
  output$test<-DT::renderDataTable({
    inner_join(primordial_thinking_library, freq(), by = c("word" = "word")) %>% group_by(type_of_thinking) %>% rename(word_frequency = value)
  })
  
}
  
shinyApp(ui=ui, server=server)