# Load the necessary libraries. 
library(shiny)
library(googleAuthR)       # For authentication
library(googleAnalyticsR)  # How we actually get the Google Analytics data

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")
options(googleAuthR.redirect = "https://gilligan.shinyapps.io/site-search/")

library(tidyverse)         # Includes dplyr, ggplot2, and others; very key!
library(knitr)             # Nicer looking tables
library(DT)                # Interactive tables
library(tidytext)          # Tidy text!
library(SnowballC)         # Mainly for stemming the search terms
library(wordcloud)         # Word cloud creation
library(RColorBrewer)      # Get some palettes to use with the word cloud
library(topicmodels)       # For the topic modeling using LDA

## ui.R
ui <- fluidPage(title = "Site Search Analysis with Google Analytics",
                tags$h2("Site Search Analysis with Google Analytics"),
                paste("This requires a site that has site search and has the typical configuration of the",
                      "capture of search terms with Google Analytics. It's purely based on a search volume",
                      "analysis. Hat tips to Sebastien Brodeur, Nancy Koons, and Julia Silge for their",
                      "contributions of the ideas that are used here."),
                sidebarLayout(
                  sidebarPanel(tags$h4("Select Base Data Parameters"),
                               # Account/Property/View Selection
                               authDropdownUI("auth_menu",
                                              inColumns = FALSE),
                               # Date Range Selection
                               dateRangeInput("date_selection", 
                                              label = "Select date range:",
                                              start = Sys.Date()-30,
                                              end = Sys.Date()-1),
                               # Whether or not to enable anti-sampling
                               checkboxInput("anti_sampling",
                                             label = "Include anti-sampling (slows down app a bit).",
                                             value = TRUE),
                               # Action button. We want the user to control when the
                               # underlying call to Google Analytics occurs.
                               tags$div(style="text-align: center",
                                        actionButton("query_data", "Get/Refresh Data!", 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                               
                               tags$hr(),
                               tags$h4("Adjust the Results"),
                               
                               # Stopword language selection. Allows selecting multiple languages
                               selectInput("stopword_lang",
                                           label = "Language(s) for stopwords:",
                                           choices = c("Danish" = "da",
                                                       "English" = "en",
                                                       "French" = "fr",
                                                       "German" = "de",
                                                       "Hungarian" = "hu",
                                                       "Spanish" = "es"),
                                           selected = "en"),
                               
                               tags$hr(),
                               tags$h4("Wordcloud Settings"),
                               
                               # Additional words to exclude from the word clouds
                               textInput("exclude_words",
                                         label = "Additional words to exclude:",
                                         value = ""),
                               
                               # The minimum word frequency to include in the wordcloud
                               sliderInput("min_frequency",
                                           label = "Minimum word frequency to include:",
                                           min = 1,
                                           max = 50,
                                           value = 2,
                                           step = 1),
                               
                               tags$hr(),
                               tags$h4("Topic Modeling"),
                               
                               # Number of topics to suss out of the data
                               sliderInput("num_topics",
                                           label = "# of topics to find:",
                                           min = 1,
                                           max = 5,
                                           value = 2,
                                           step = 1)),
                  
                  mainPanel(tags$h3("Results"),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Overall Word Cloud",
                                                 plotOutput("wordcloud")),
                                        tabPanel("Topics Word Clouds"),
                                        tabPanel("Question Searches",
                                                 dataTableOutput("questions")),
                                        tabPanel("Term-Frequency Table",
                                                 dataTableOutput("term_frequency")),
                                        tabPanel("Raw Data",
                                                 dataTableOutput("ga_data"))))
                )
)

## server.R
server <- function(input, output, session){
  
  # Create a non-reactive access token
  gar_shiny_auth(session)
  
  # Populate the Account/Property/View dropdowns and return whatever the
  # selected view ID is
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account_list)
  
  # Reactive function to pull the data.
  get_ga_data <- reactive({
    
    # Only pull the data if the "Get Data" button is clicked
    input$query_data
    
    # Pull the data.
    isolate(google_analytics(viewId = view_id(),
                             date_range = input$date_selection,
                             metrics = "searchUniques",
                             dimensions = "searchKeyword",
                             anti_sample = input$anti_sampling))
  })
  
  # Reactive function to get the cleaned up search data
  get_search_data_clean <- reactive({
    
    # Get the raw data
    ga_data <- get_ga_data()
    
    # Unnest it -- put each word on its own row and then collapse the individual
    # words. This will also make everything lowercase and strip punctuation!
    search_data_clean <- ga_data %>% 
      unnest_tokens(search_term, searchKeyword) %>% 
      group_by(search_term) %>% 
      summarise(searches = sum(searchUniques)) %>% 
      select(search_term, searches) %>% 
      ungroup() %>% 
      arrange(-searches)
    
    # Remove the stop words. 1) get the stopwords, 2) remove 'em. There may be
    # multiple languages of stopwords selected, so looping through those.
    if(length(input$stopwords_lang > 0)){
      for(lang in input$stopwords_lang){
        # Get the stopwords for the language
        stop_words <- get_stopwords(language = lang) %>% select(word)
        search_data_clean <- search_data_clean %>%
          anti_join(stop_words, by = c(search_term = "word"))
      }
    }
    
    # Convert UTF-8 to ASCII (needed because all hell starts to break loose if you 
    # try to text-mine multibyte). So, we're going to try to convert everything to
    # ASCII. For some...this will fail and return NA. So, we'll then just remove
    # the NA rows
    search_data_clean <- search_data_clean %>%
      mutate(search_term = iconv(search_term, "UTF-8", "ASCII")) %>% 
      filter(!is.na(search_term))
    
    # Perform stemming.
    search_data_clean <- search_data_clean %>% 
      mutate(search_term_stem = wordStem(search_term))
    
    # Go ahead and find the most popular un-stemmed word for each stemmed word.
    # That will make the results look more "normal" to the casual viewer. We don't want
    # to have any ties, so we're going to somewhat arbitrarily break any ties by adding
    # the row number / 1000000 to each of the search counts first (We'll toss this later)
    search_data_clean_top_term <- search_data_clean %>% 
      mutate(searches = searches + row_number()/1000000) %>% 
      group_by(search_term_stem) %>% 
      top_n(1, searches) %>% 
      select(-searches)
    
    # Join that back to search data after totalling the searches by the stemmed term.
    search_data_clean <- search_data_clean %>% 
      group_by(search_term_stem) %>% 
      summarise(searches = sum(searches)) %>% 
      left_join(search_data_clean_top_term) %>% 
      ungroup() %>% 
      select(search_term_stem, search_term, searches) %>% 
      arrange(-searches)
    
    # Convert the list of additional exclusion words to a vector. There may or may not be
    # spaces after the commas separating the terms.
    exclude_words <- strsplit(input$exclude_words, ",") %>%
      flatten() %>% as.character() %>% gsub(" ", "", .)
    
    # Remove any additional "remove words" specified
    search_data_clean <-  search_data_clean %>%
      filter(!search_term_stem %in% exclude_words)
  })
  
  # Output the raw data
  output$ga_data <- DT::renderDataTable({
    get_ga_data() %>% 
      arrange(-searchUniques) %>% 
      datatable(colnames = c("Search Term", "Searches"),  rownames = FALSE)
  })
  
  # Output the questions
  output$questions <- DT::renderDataTable({
    get_ga_data() %>% 
      arrange(-searchUniques) %>% 
      filter(grepl("^who|^what|^why|^what|^when|^where|^how.*", searchKeyword)) %>% 
      datatable(colnames = c("Question", "Searches"),  rownames = FALSE)
  })
  
  # Output the term-frequency table
  output$term_frequency <- DT::renderDataTable({
    get_search_data_clean() %>% 
      select(search_term, searches) %>% 
      datatable(colnames = c("Search Term", "Searches"),
                rownames = FALSE)
  })
  
  # Output the wordcloud
  output$wordcloud <- renderPlot({
    
    # Set a seed for reproducibility
    set.seed(1971)
    
    # Get the search data
    search_data_clean <- get_search_data_clean()
    
    # Set a color palette
    color_palette <- rev(brewer.pal(8,"Spectral")) 
    
    # Generate the word cloud!
    wordcloud(words = search_data_clean$search_term, 
              freq = search_data_clean$searches,
              scale = c(5.5,0.6),
              min.freq = input$min_frequency,
              max.words = 500, 
              random.order = FALSE,
              rot.per = .0,
              colors = color_palette)
  })
}

# shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
