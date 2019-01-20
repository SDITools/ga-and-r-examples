# THIS EXAMPLE DOES NOT CURRENTLY WORK. IT WILL BE UPDATED AS TIME ALLOWS, ALTHOUGH
# IT IS ALWAYS GOING TO BE SOMEWHAT HAMPERED BY LIMITATIONS OF USING THE FREE
# TWITTER API.

# Load the necessary libraries. 
library(shiny)

# Ideally, we would use rtweet, but rtweet does not play nice with Shiny, so twitteR it is!
library(twitteR)           # How we actually get the Twitter data
library(tidyverse)         # Includes dplyr, ggplot2, and others; very key!
library(gridExtra)         # Grid / side-by-side plot layoutslibrary
library(knitr)             # Nicer looking tables
library(DT)                # Interactive tables
library(tidytext)          # Tidy text!
library(SnowballC)         # Mainly for stemming the followers terms
library(wordcloud)         # Word cloud creation
library(RColorBrewer)      # Get some palettes to use with the word cloud
library(topicmodels)       # For the topic modeling using LDA


## ui.R
ui <- fluidPage(title = "Twitter Follower Analysis",
                tags$head(includeScript("gtm.js")),
                tags$h2("Twitter Follower Analysis*"),
                tags$div("Enter a username and then text mine their followers' descriptions!"),
                tags$br(),
                sidebarLayout(
                  sidebarPanel(tags$h4("Select Base Data Parameters"),
                               # Get the username
                               textInput("tw_account",
                                         label = "Enter a Twitter username to analyze:",
                                         value = "analyticshour"),
                               # Action button. We want the user to control when the
                               # underlying call to Twitter occurs.
                               tags$div(style="text-align: center",
                                        actionButton("query_data", "Get/Refresh Data!", 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                               
                               tags$hr(),
                               tags$h4("Refine the Results"),
                               
                               # Stopword language selection. Allows selecting multiple languages
                               selectInput("stopwords_lang",
                                           label = "Language(s) for stopwords:",
                                           choices = c("Danish" = "da",
                                                       "English" = "en",
                                                       "French" = "fr",
                                                       "German" = "de",
                                                       "Hungarian" = "hu",
                                                       "Spanish" = "es"),
                                           multiple = TRUE,
                                           selected = "en"),
                               
                               # Additional words to exclude from the word clouds
                               textInput("exclude_words",
                                         label = paste("Additional words to exclude (from the term-frequency",
                                                       "table, word cloud, and topic analysis):",
                                                       value = ""))
                  ),
                  
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel("Raw Data",
                                                 tags$br(),
                                                 tags$div("This is the raw data returned from Twitter."),
                                                 tags$br(),
                                                 dataTableOutput("followers_details")),
                                        tabPanel("Term-Frequency Table",
                                                 tags$br(),
                                                 tags$div("These are the results after all descriptions were split into",
                                                          "individual words, the words were stemmed and combined,",
                                                          "stopwords were removed, and any additional exclusion words",
                                                          "were removed. These are the words used to create the word cloud."),
                                                 tags$br(),
                                                 dataTableOutput("term_frequency")),
                                        tabPanel("Overall Word Cloud",
                                                 tags$br(),
                                                 tags$div("The word cloud based on the cleaned up data set."),
                                                 tags$br(),
                                                 fluidRow(
                                                   # Number of topics to suss out of the data
                                                   # The minimum word frequency to include in the wordcloud
                                                   column(4, sliderInput("min_frequency",
                                                                         label = "Minimum # of occurrences to include:",
                                                                         min = 1, max = 50, value = 2, step = 1)),
                                                   column(4,  sliderInput("overall_min_size",
                                                                          label = "Minimum word size:",
                                                                          min = 1,  max = 8, value = 2, step = 0.5)),
                                                   column(4,  sliderInput("overall_max_size",
                                                                          label = "Maximum word size:",
                                                                          min = 1,  max = 8, value = 5.5, step = 0.5))),
                                                 fluidRow(plotOutput("wordcloud", height = "800px"))),
                                        tabPanel("Topics Word Clouds",
                                                 tags$br(),
                                                 tags$div("This is a little bit of unsupervised text mining using Latent Dirichlet",
                                                          "Allocation (LDA). Based on the number of topics you select below, the LDA",
                                                          "model establishes a set of topics and then assigns a probability that each",
                                                          "word in the data set would appear in that topic. The word clouds",
                                                          "show the terms that had the highest probability of showing up in each topic",
                                                          "(and excludes terms that had less than a 0.1% chance of appearing in any topic."),
                                                 tags$br(),
                                                 fluidRow(
                                                   column(2, sliderInput("num_topics",
                                                                         label = "# of topics to find:",
                                                                         min = 2, max = 6, value = 2, step = 1)),
                                                   column(3, sliderInput("term_topic_probability",
                                                                         label = "Strength of term fit:",
                                                                         min = 0.001,  max = 0.01, value = 0.001, step = 0.001)),
                                                   column(3, sliderInput("topics_min_frequency",
                                                                         label = "Minimum # of occurrences to include:",
                                                                         min = 1, max = 50, value = 2, step = 1)),
                                                   column(2, sliderInput("topics_min_size",
                                                                         label = "Min. word size:",
                                                                         min = 1,  max = 8, value = 1, step = 0.5)),
                                                   column(2, sliderInput("topics_max_size",
                                                                         label = "Max word size:",
                                                                         min = 1,  max = 8, value = 3.5, step = 0.5))),
                                                 fluidRow(plotOutput("topic_wordclouds", height = "600px"))
                                        )))),
                tags$hr(),
                tags$div("*This app is part of a larger set of apps that demonstrate some uses of R in conjunction",
                         "with Google Analytics (and Twitter). For the code for this app, as well as an R Notebook",
                         "that includes more details, see:", tags$a(href = "https://github.com/SDITools/ga-and-r-examples/",
                                                                    "https://github.com/SDITools/ga-and-r-examples/"),"."),
                tags$br()
)


## server.R
server <- function(input, output, session){
  
  # Authenticate. Values need to be added here!
  setup_twitter_oauth(consumer_key = "", 
                      consumer_secret = "", 
                      access_token = "", 
                      access_secret = "")
  
  # Reactive function to pull the data.
  get_followers_details <- reactive({
    
    # Only pull the data if the "Get Data" button is clicked
    input$query_data
    
    # Get a list of all followers
    user <- isolate(getUser(input$tw_account))
    
    # Get the details for those followers
    followers_details <- isolate(user$getFollowers(n=100) %>% twListToDF())

  })
  
  # Reactive function to get the cleaned up description data
  get_description_data_clean <- reactive({
    
    # Get the raw data
    followers_details <- get_followers_details()
    
    # cat(names(followers_details, "/n"))
    
    # Unnest it -- put each word on its own row and then collapse the individual
    # words. This will also make everything lowercase and strip punctuation!
    followers_data_clean <- followers_details %>% 
      unnest_tokens(description_term, description) %>% 
      mutate(occurrences = 1) %>%      # Unlike site search, everything we pull occurs "once" each time it appears
      dplyr::select(description, occurrences) %>% 
      ungroup() %>% 
      arrange(-occurrences)
    
    # Remove the stop words. 1) get the stopwords, 2) remove 'em. There may be
    # multiple languages of stopwords selected, so looping through those.
    if(length(input$stopwords_lang > 0)){
      for(lang in input$stopwords_lang){
        # Get the stopwords for the language
        stop_words <- get_stopwords(language = lang) %>% dplyr::select(word)
        description_data_clean <- description_data_clean %>%
          anti_join(stop_words, by = c(description = "word"))
      }
    }
    
    # Convert UTF-8 to ASCII (needed because all hell starts to break loose if you 
    # try to text-mine multibyte). So, we're going to try to convert everything to
    # ASCII. For some...this will fail and return NA. So, we'll then just remove
    # the NA rows
    description_data_clean <- description_data_clean %>%
      mutate(description = iconv(description, "UTF-8", "ASCII")) %>% 
      filter(!is.na(description))
    
    # Perform stemming.
    description_data_clean <- description_data_clean %>% 
      mutate(description_stem = wordStem(description))
    
    # Go ahead and find the most popular un-stemmed word for each stemmed word.
    # That will make the results look more "normal" to the casual viewer. We don't want
    # to have any ties, so we're going to somewhat arbitrarily break any ties by adding
    # the row number / 1000000 to each of the description counts first (We'll toss this later)
    description_data_clean_top_term <- description_data_clean %>% 
      mutate(occurrences = occurrences + row_number()/1000000) %>% 
      group_by(description_stem) %>% 
      top_n(1, occurrences) %>% 
      dplyr::select(-occurrences)
    
    # Join that back to description data after totalling the occurrences by the stemmed term.
    description_data_clean <- description_data_clean %>% 
      group_by(description_stem) %>% 
      summarise(occurrences = sum(occurrences)) %>% 
      left_join(description_data_clean_top_term) %>% 
      ungroup() %>% 
      dplyr::select(description_stem, description, occurrences) %>% 
      arrange(-occurrences)
    
    # Convert the list of additional exclusion words to a vector. There may or may not be
    # spaces after the commas separating the terms.
    # Remove any of the exclusion terms that are entered.
    if(!is.null(input$exclude_words)){
      # Take the comma-delimited list of terms and split them out to be a
      # character vector. The ", ?" regEx is so that this will work with
      # or without a space following the comma
      exclude_words <- unlist(strsplit(input$exclude_words,", ?"))

      # Remove any additional "remove words" specified
      description_data_clean <-  description_data_clean %>%
        filter(!description %in% exclude_words)
    }
  })
  
  # Reactive function to do the LDA topic modeling
  get_description_topics_and_terms <- reactive({
    
    # Get the cleaned up description data
    description_data_clean <- get_description_data_clean()
    
    # Cast the term frequency matrix into a document term matrix. We're considering this all one 
    # "document" so we're just hardcoding a "1" for that
    description_data_dtm <- description_data_clean %>% 
      mutate(doc = 1) %>% 
      cast_dtm(doc, description, occurrences)
    
    # Run LDA. Setting a seed for reproducibility
    description_lda <- LDA(description_data_dtm, k = input$num_topics, control = list(seed = 1120))
    
    # Assign a probability of each term being in each of the topics
    description_topics <- tidy(description_lda, matrix = "beta")
    
    # For each term, assign it to the topic for which it has the highest beta. This diverges
    # from the approach described at tidytextmining.com, but it seems like a reasonably legit
    # thing to do.
    description_topics_and_terms <- description_topics %>%
      group_by(term) %>% 
      top_n(1, beta) %>% 
      ungroup() %>% 
      arrange(topic, -beta) %>% 
      left_join(description_data_clean, by = c(term = "description"))
  })
  
  # Output the raw data
  output$followers_details <- DT::renderDataTable({
    get_followers_details() %>% 
      dplyr::select(screenName, description, followersCount) %>% 
      arrange(-followersCount) %>%
      datatable(colnames = c("User","Description", "Followers"),  rownames = FALSE)
  })
  
  
  # Output the term-frequency table
  output$term_frequency <- DT::renderDataTable({
    get_description_data_clean() %>% 
      dplyr::select(description, occurrences) %>% 
      datatable(colnames = c("Description Term", "Occurrences"),
                rownames = FALSE)
  })
  
  # Set a seed for reproducibility
  set.seed(1971)
  
  # Set a color palette
  color_palette <- rev(brewer.pal(8,"Spectral")) 
  
  # Output the wordcloud
  output$wordcloud <- renderPlot({
    
    # Get the description data
    description_data_clean <- get_description_data_clean()
    
    # Generate the word cloud!
    wordcloud(words = description_data_clean$description, 
              freq = description_data_clean$occurrences,
              scale = c(input$overall_max_size, input$overall_min_size),
              min.freq = input$min_frequency,
              max.words = 500, 
              random.order = FALSE,
              rot.per = .0,
              colors = color_palette)
  })
  
  # This gets a little janky, in that we can't really do recursive / variable
  # topic counts. So, instead, we're going to have an output for each of FIVE
  # topics, but then have those return empty results if fewer topics are actually
  # selected. There *may* be some inefficiencies here, but I couldn't get anything
  # moved out of this to avoid the repetition.
  
  # Topic #1  
  wordcloud_1 <- reactive({
    # Populate description topics and terms. 
    topic_data <- get_description_topics_and_terms()  %>% 
      filter(topic == 1 &  beta > input$term_topic_probability)
    # Generate the word cloud!
    wordcloud(words = topic_data$term, freq = topic_data$occurrences,
              scale=c(input$topics_max_size,input$topics_min_size), 
              min.freq=input$topics_min_frequency, max.words=500, 
              random.order=FALSE, rot.per=.0, colors=color_palette)
  })
  
  # Topic #2
  wordcloud_2 <- reactive({
    # Populate description topics and terms. 
    topic_data <- get_description_topics_and_terms()  %>% 
      filter(topic == 2 &  beta > input$term_topic_probability)
    # Generate the word cloud!
    wordcloud(words = topic_data$term, freq = topic_data$occurrences,
              scale=c(input$topics_max_size,input$topics_min_size), 
              min.freq=input$topics_min_frequency, max.words=500, 
              random.order=FALSE, rot.per=.0, colors=color_palette)
  })
  
  # For 3-6, the slider might be set below them, so we have to check to see before trying
  # to generate a word cloud
  
  # Topic #3
  wordcloud_3 <- reactive({
    if(input$num_topics >= 3){
      # Populate description topics and terms. 
      topic_data <- get_description_topics_and_terms()  %>% 
        filter(topic == 3 &  beta > input$term_topic_probability)
      # Generate the word cloud!
      wordcloud(words = topic_data$term, freq = topic_data$occurrences,
                scale=c(input$topics_max_size,input$topics_min_size), 
                min.freq=input$topics_min_frequency, max.words=500, 
                random.order=FALSE, rot.per=.0, colors=color_palette)
    } else {
      NULL
    }
  })
  
  # Topic #4
  wordcloud_4 <- reactive({
    if(input$num_topics >= 4){
      # Populate description topics and terms. 
      topic_data <- get_description_topics_and_terms()  %>% 
        filter(topic == 4 &  beta > input$term_topic_probability)
      # Generate the word cloud!
      wordcloud(words = topic_data$term, freq = topic_data$occurrences,
                scale=c(input$topics_max_size,input$topics_min_size), 
                min.freq=input$topics_min_frequency, max.words=500, 
                random.order=FALSE, rot.per=.0, colors=color_palette)
    } else {
      NULL
    }
  })
  
  # Topic #5
  wordcloud_5 <- reactive({
    if(input$num_topics >= 5){
      # Populate description topics and terms. 
      topic_data <- get_description_topics_and_terms()  %>% 
        filter(topic == 5 &  beta > input$term_topic_probability)
      # Generate the word cloud!
      wordcloud(words = topic_data$term, freq = topic_data$occurrences,
                scale=c(input$topics_max_size,input$topics_min_size), 
                min.freq=input$topics_min_frequency, max.words=500, 
                random.order=FALSE, rot.per=.0, colors=color_palette)
    } else {
      NULL
    }
  })
  
  # Topic #6
  wordcloud_6 <- reactive({
    if(input$num_topics >= 6){
      # Populate description topics and terms. 
      topic_data <- get_description_topics_and_terms()  %>% 
        filter(topic == 6 &  beta > input$term_topic_probability)
      # Generate the word cloud!
      wordcloud(words = topic_data$term, freq = topic_data$occurrences,
                scale=c(input$topics_max_size,input$topics_min_size), 
                min.freq=input$topics_min_frequency, max.words=500, 
                random.order=FALSE, rot.per=.0, colors=color_palette)
    } else {
      NULL
    }
  })
  
  # Output the grid of wordclouds
  output$topic_wordclouds <- renderPlot({
    # Layout out a 2x3 grid with all of the wordclouds and return that as 
    # the plot.
    par(mfrow=c(2,3)) # for 1 row, 2 cols
    wordcloud_1()
    wordcloud_2()
    wordcloud_3() 
    wordcloud_4()
    wordcloud_5()
    wordcloud_6()
  })
}

# shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
shinyApp(ui = ui, server = server)
