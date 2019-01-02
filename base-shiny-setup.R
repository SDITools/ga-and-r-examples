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
                            plotlyOutput("upvs_by_day_cum"))
                ))

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
  
}

# shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
