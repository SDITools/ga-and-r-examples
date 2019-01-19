# Load the necessary libraries. 
library(shiny)
library(googleAuthR)       # For authentication
library(googleAnalyticsR)  # How we actually get the Google Analytics data

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")
options(googleAuthR.redirect = "https://gilligan.shinyapps.io/time-normalized/")

library(tidyverse)         # Includes dplyr, ggplot2, and others; very key!
library(knitr)             # Nicer looking tables
library(plotly)            # We're going to make the charts interactive
library(DT)                # Interactive tables
library(scales)            # Useful for some number formatting in the visualizations

## ui.R
ui <- fluidPage(title = "Time-Normalized Pageviews",
                tags$head(includeScript("gtm.js")),
                tags$h2("Time-Normalized Pageviews"),
                sidebarLayout(
                  sidebarPanel(tags$h3("Select a view:"),
                               # Account/Property/View Selection
                               authDropdownUI("auth_menu",
                                              inColumns = FALSE),
                               # Date Range Selection
                               dateRangeInput("date_selection", 
                                              label = "Select date range:",
                                              start = Sys.Date()-365,
                                              end = Sys.Date()-1),
                               # Page filter (regEx)
                               textInput("filter_regex",
                                         label = "Enter regEx to filter to the pages of interest:",
                                         value = ".*"),
                               # The minimum number of pageviews required for a page to be deemed "live"
                               numericInput("first_day_pageviews_min",
                                            label = "Minimum # of pageviews on a given day for the page to be deemed 'launched':",
                                            value = 5,
                                            min = 1),
                               # The number of pages to include in the output
                               numericInput("total_pages_included",
                                            label = "Total pages to include in the output:",
                                            value = 20,
                                            min = 1),
                               # The number of "days from launch" to assess
                               numericInput("days_live_range",
                                            label = "# of days post-launch to include:",
                                            value = 30,
                                            min = 7),
                               # Whether or not to enable anti-sampling
                               checkboxInput("anti_sampling",
                                             label = "Include anti-sampling (slows down app a bit).",
                                             value = TRUE)),
                   mainPanel(tags$h3("Results"),
                            tags$hr(),
                            plotlyOutput("upvs_by_day"),
                            tags$hr(),
                            plotlyOutput("upvs_by_day_cum"))),
                tags$hr(),
                tags$div("*This app is part of a larger set of apps that demonstrate some uses of R in conjunction",
                         "with Google Analytics (and Twitter). For the code for this app, as well as an R Notebook",
                         "that includes more details, see:", tags$a(href = "https://github.com/SDITools/ga-and-r-examples/",
                                                                    "https://github.com/SDITools/ga-and-r-examples/"),"."),
                tags$br()
)


## server.R
server <- function(input, output, session){
  
  # Create a non-reactive access token
  gar_shiny_auth(session)
  
  # Populate the Account/Property/View dropdowns and return whatever the
  # selected view ID is
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account_list)
  
  # Reactive function to build the page filter object
  page_filter <- reactive({
    # Create a dimension filter object. See ?dim_filter() for details. 
    page_filter_object <- dim_filter("pagePath", 
                                     operator = "REGEXP",
                                     expressions = input$filter_regex)
    # Now, put that filter object into a filter clause.
    filter_clause_ga4(list(page_filter_object),
                      operator = "AND")
  })
  
  # Reactive function to pull the data.
  ga_data <- reactive({
    # Pull the data. 
    google_analytics(viewId = view_id(),
                     date_range = input$date_selection,
                     metrics = "uniquePageviews",
                     dimensions = c("date","pagePath"),
                     dim_filters = page_filter(),
                     anti_sample = input$anti_sampling)
  })
  
  # Function to do the data normalization
  ga_data_normalized <- reactive({
    
    # Don't try to normalize anything until there is data to be worked with
    req(ga_data())
    
    # Get the data from GA
    ga_data <- ga_data()
    
    # Function to filter and then normalize a single page
    normalize_date_start <- function(page){
      
      # Filter all the data to just be the page being processed
      ga_data_single_page <- ga_data %>% filter(pagePath == page)
      
      # Find the first value in the result that is greater than first_day_pageviews_min. 
      first_live_row <- min(which(ga_data_single_page$uniquePageviews > input$first_day_pageviews_min))
      
      # If the "first_live_row" is Inf, then none of the traffic for the page on any given day
      # exceeded the first_day_pageviews_min value, so we're going to go ahead and exit the function
      # with a NULL result
      if(first_live_row == Inf){return(NULL)}
      
      # Filter the data to start with that page
      ga_data_single_page <- ga_data_single_page[first_live_row:nrow(ga_data_single_page),]
      
      # As the content ages, there may be days that have ZERO traffic. Those days won't show up as
      # rows at all in our data. So, we actually need to create a data frame that includes
      # all dates in the range from the "launch" until the last day traffic was recorded. 
      normalized_results <- data.frame(date = seq.Date(from = min(ga_data_single_page$date), 
                                                       to = max(ga_data_single_page$date), 
                                                       by = "day"),
                                       days_live = seq(min(ga_data_single_page$date):
                                                         max(ga_data_single_page$date)),
                                       page = page,
                                       stringsAsFactors = FALSE) %>% 
        
        # Join back to the original data to get the uniquePageviews
        left_join(ga_data_single_page) %>%
        
        # Replace the "NAs" (days in the range with no uniquePageviews) with 0s (because 
        # that's exactly what happened on those days!)
        mutate(uniquePageviews = ifelse(is.na(uniquePageviews), 0, uniquePageviews)) %>% 
        
        # We're going to plot both the daily pageviews AND the cumulative total pageviews,
        # so let's add the cumulative total
        mutate(cumulative_uniquePageviews = cumsum(uniquePageviews)) %>% 
        
        # Grab just the columns we need for our visualization!
        select(page, days_live, uniquePageviews, cumulative_uniquePageviews)
    }
    
    # We want to run the function above on each page in our data set. 
    pages_list <- ga_data %>% 
      group_by(pagePath) %>% summarise(total_traffic = sum(uniquePageviews)) %>% 
      top_n(input$total_pages_included)
    
    # The first little bit of magic can now occur. We'll run our normalize_date_start function on
    # each value in our list of pages and get a data frame back that has our time-normalized
    # traffic by page!
    ga_data_normalized <- map_dfr(pages_list$pagePath, normalize_date_start)
    
    # We specified earlier -- in the `days_live_range` object -- how many "days from launch" we
    # actually want to include, so let's do one final round of filtering to only include those rows.
    ga_data_normalized <- ga_data_normalized %>% filter(days_live <= input$days_live_range)
    
    return(ga_data_normalized)
  })
  
  # The actual visualizations and their output
  
  # Unique Pageviews by Day
  output$upvs_by_day <- renderPlotly({
    
    # Make sure we've got data to work with
    req(ga_data_normalized())
    
    # Get the plot data
    ga_data_normalized <- ga_data_normalized()
    
    # Create the plot
    gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = uniquePageviews, color=page)) +
      geom_line() +                                          # The main "plot" operation
      scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
      labs(title = "Unique Pageviews by Day from Launch",
           x = "# of Days Since Page Launched",
           y = "Unique Pageviews") +
      theme_light() +                                        # Clean up the visualization a bit
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            panel.grid.major.y = element_line(color = "gray80"),
            axis.ticks = element_blank())
    
    # Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
    ggplotly(gg) %>% layout(autosize=TRUE)
  })
  
  # Unique Pageviews by Day - Cumulative from launch
  output$upvs_by_day_cum <- renderPlotly({
    
    # Make sure we've got data to work with
    req(ga_data_normalized())
    
    # Get the plot data
    ga_data_normalized <- ga_data_normalized()
    
    gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumulative_uniquePageviews, color=page)) +
      geom_line() +                                          # The main "plot" operation
      scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
      labs(title = "Cumulative Unique Pageviews by Day from Launch",
           x = "# of Days Since Page Launched",
           y = "Cumulative Unique Pageviews") +
      theme_light() +                                        # Clean up the visualization a bit
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            panel.grid.major.y = element_line(color = "gray80"),
            axis.ticks = element_blank())
    
    # Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
    ggplotly(gg) %>% layout(autosize=TRUE)
  })
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
