# Load the necessary libraries. 
library(shiny)
library(googleAuthR)       # For authentication
library(googleAnalyticsR)  # How we actually get the Google Analytics data

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")
options(googleAuthR.redirect = "https://gilligan.shinyapps.io/regression/")

library(tidyverse)         # Includes dplyr, ggplot2, and others; very key!
library(knitr)             # Nicer looking tables
library(plotly)            # We're going to make the charts interactive
library(DT)                # Interactive tables
library(scales)            # Useful for some number formatting in the visualizations
library(lubridate)         # For working with dates a bit

# Define the base theme for visualizations
theme_base <- theme_bw() +
  theme(text = element_text(family="Nunito"),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.margin = margin(1.5,0,0,0,"cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 10, hjust = 0.5),
        axis.title.y = element_blank(),
        axis.line.x = element_line(color = "gray50"),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.5, colour = "gray90"),
        panel.grid.minor = element_blank())

# And, a theme for the time-series decomposition
theme_sparklines <- theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.justification = "center",
        strip.text.x = element_text(face = "bold", size = 14, colour = "grey10", family="Nunito"),
        strip.text.y = element_text(face = "bold", size = 14, colour = "grey10", 
                                    angle = 180, hjust=1, family="Nunito"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"in"),
        panel.background = element_rect(fill = NA, color = NA))

## ui.R
ui <- fluidPage(title = "Anomaly Detection through Holt-Winters Forecasting",
                tags$head(includeScript("gtm.js")),
                tags$h2("Anomaly Detection through Holt-Winters Forecasting*"),
                tags$div(paste("Select a Google Analytics view and date range and then pull the data. From there, explore",
                               "time-series decomposition and forecasting as a means of identifying anomalies!")),
                tags$br(),
                sidebarLayout(
                  sidebarPanel(tags$h4("Select Base Data Parameters"),
                               # Account/Property/View Selection
                               authDropdownUI("auth_menu",
                                              inColumns = FALSE),
                               # Overall Date Range Selection
                               dateRangeInput("assessment_period", 
                                              label = "Select the overall date range to use:",
                                              start = Sys.Date()-90,
                                              end = Sys.Date()-1),
                               # Assessment Days
                               sliderInput("check_period", 
                                           label = "How many of those days do you want to check for anomalies?",
                                           min = 3, max = 14, value = 7),
                               # Whether or not to enable anti-sampling
                               checkboxInput("anti_sampling",
                                             label = "Include anti-sampling (slows down app a bit).",
                                             value = TRUE),
                               # Action button. We want the user to control when the
                               # underlying call to Google Analytics occurs.
                               tags$div(style="text-align: center",
                                        actionButton("query_data", "Get/Refresh Data!", 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                               tags$br(),
                               tags$hr()),
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel("Base Data",
                                                 tags$br(),
                                                 tags$div(paste("This is the base data and a visualization of the data",
                                                                "that you queried. It should look pretty familiar!")),
                                                 tags$br(),
                                                 plotlyOutput("base_data_plot", height = "400px")),
                                        tabPanel("Training vs. Assessment",
                                                 tags$br(),
                                                 tags$div(paste("We've split the data into two groups: the data that will be",
                                                                "used to train the model, and the data that we are actually",
                                                                "evaluating for anomalies")),
                                                 tags$br(),
                                                 plotlyOutput("train_vs_assess_plot", height = "400px")),
                                        tabPanel("Time-Series Decomposition",
                                                 tags$br(),
                                                 tags$div(paste("This is the time-series decomposition of the training data.",
                                                                "We've broken out the actual data into three components: seasonal",
                                                                "with a weekly 'season', trend, and random (noise).")),
                                                 tags$br(),
                                                 plotlyOutput("time_series_decomp_plot")),
                                        # tabPanel("Forecast with a Prediction Interval",
                                        #          tags$br(),
                                        #          tags$div(paste("This is the raw summary of a stepwise regression model using",
                                        #                         "the data set on the 'Dummies' tab. It's not super pretty, is it?")),
                                        #          tags$br(),
                                        #          plotlyOutput("final_assessment"))
                                        tabPanel("Master Data Table",
                                                 dataTableOutput("data_table"))
                  ))),
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
  
  # Reactive function to pull the data.
  get_ga_data <- reactive({
    
    # Only pull the data if the "Get Data" button is clicked
    input$query_data
    
    # Pull the data. Go ahead and shorten the weeday names
    # Pull the data. See ?google_analytics_4() for additional parameters. The anti_sample = TRUE
    # parameter will slow the query down a smidge and isn't strictly necessary, but it will
    # ensure you do not get sampled data.
    isolate(google_analytics(viewId = view_id(),
                             date_range = input$assessment_period,
                             metrics = "sessions",
                             dimensions = "date",
                             anti_sample = input$anti_sampling)) 
  })
  
  # Determine how many rows of the data will be used to build the forecast. This
  # is everything except the last week.
  rowcount_forecast <- reactive(nrow(get_ga_data()) - input$check_period)
  
  # Also figure out the date where the cutoff is between training and forecast.
  # We actually want to shift this over a little bit to fall between two points when we plot
  cutoff_date <- reactive({
    
    # Get the data
    ga_data <- get_ga_data()
    
    # Figure out the cutoff date.
    cutoff_date <- ga_data$date[rowcount_forecast()] 
    
    # This is the "shifting it over a bit" piece
    cutoff_date <- (2*as.numeric(cutoff_date) + 1)/2
  })
  
  # Make a data set that removes the "rows to be evaluated." This will get 
  # used both to generate the time series for the forecast as well as for modeling
  get_ga_data_training <- reactive({
    get_ga_data() %>%
      top_n(-rowcount_forecast(), wt = date) 
  })
  
  # Get the date values for the forecast period
  dates_forecast <- reactive({
    get_ga_data() %>%
      top_n(input$check_period, wt = date) %>%
      dplyr::select(date)
  })
  
  # Make a time-series object using the data for the training period. This
  # is what we'll use to build the forecast
  get_ga_data_ts <- reactive({
    get_ga_data_training() %>%
      dplyr::select(sessions) %>% 
      ts(frequency = 7)
  })
  
  # Start building out our master data for plotting by adding a column that
  # has just the data being used for the training
  get_ga_data_plot <- reactive({
    ga_data_plot <- get_ga_data() %>%
      left_join(get_ga_data_training(), by = c(date = "date"))
    
    # Rename columns to be a bit clearer
    names(ga_data_plot) <- c("date", "sessions_all", "sessions_training")
    
    # Add a column that is just the actuals data of interest
    ga_data_plot <- ga_data_plot %>%
      mutate(sessions_assess = ifelse(is.na(sessions_training), sessions_all, NA))
  })
  
  ## Outputs
  
  ##### TEMP: Master data table
  output$data_table <- renderDataTable(get_ga_data_plot())
  
  # Output the base data plot
  output$base_data_plot <- renderPlotly({
    
    # Get the data for plotting
    ga_data_plot <- get_ga_data_plot()
    
    # Get the upper limit for the plot. We'll use this for all of the plots just for clarity
    y_max <- max(ga_data_plot$sessions_all) * 1.03
    
    # Build a plot showing just the actual data
    ga_plot <- ggplot(ga_data_plot, mapping = aes(x = date)) +
      geom_line(aes(y = ga_data_plot$sessions_all), color = "#0060AF", size = 0.75) +
      scale_y_continuous(label=comma, expand = c(0, 0), limits = c(0, y_max)) +
      labs(x= " ") +
      theme_base
    
    # Plot the data
    ggplotly(ga_plot) %>% layout(autosize=TRUE)
  })
  
  # Output the data split by training vs. assessment
  output$train_vs_assess_plot <- renderPlotly({
    
    # Get the data for plotting
    ga_data_plot <- get_ga_data_plot()
    
    # Get the upper limit for the plot. We'll use this for all of the plots just for clarity
    y_max <- max(ga_data_plot$sessions_all) * 1.03
    
    # Same plot, with the training data highlighted
    ga_plot <- ggplot(ga_data_plot, mapping = aes(x = date)) +
      geom_line(aes(y = ga_data_plot$sessions_training), color = "#0060AF", size = 0.75) +
      geom_line(aes(y = ga_data_plot$sessions_assess), color = "gray80", size = 0.75) +
      geom_vline(aes(xintercept = cutoff_date()), 
                 color = "gray40", linetype = "dashed", size = 1) +
      scale_y_continuous(label=comma, expand = c(0, 0), limits = c(0, y_max)) +
      labs(x= " ") +
      theme_base
    
    # Plot the data
    ggplotly(ga_plot) %>% layout(autosize=TRUE)
  })
  
  # Output the time-series decomposition
  output$time_series_decomp_plot <- renderPlotly({
    
    # Get the time-series data and the training data
    ga_data_ts <- get_ga_data_ts()
    ga_data_trainins <- get_ga_data_training()
    
    # Decompose the time-series data
    ga_stl <- stl(ga_data_ts,
                  s.window = "periodic",
                  t.window = 7) 
    
    # Convert that to a long format data frame
    ga_stl_df <- data.frame(Actual = ga_data_ts %>% as.data.frame()) %>% 
      cbind(ga_stl$time.series %>% as.data.frame()) %>% 
      mutate(date = ga_data_training$date) %>% 
      dplyr::select(date, 
                    Actual = x,
                    Seasonal = seasonal,
                    Trend = trend,
                    Random = remainder) %>%
      mutate(date = ga_data_training$date) %>%
      gather(key, value, -date)
    
    # We want to control the order of the output, so make key a factor
    ga_stl_df$key <- factor(ga_stl_df$key,
                            levels = c("Actual", "Seasonal", "Trend", "Random"))
    
    ## We can "decompose" that data.
    
    # Plot the values
    ga_plot <- ggplot(ga_stl_df, mapping = aes(x = date, y = value, colour = key)) +
      geom_line(size = 1) +
      facet_grid(key ~ ., scales = "free", switch = "y") +
      scale_color_manual(values=c("#0060AF", "#999999", "#999999", "#999999")) +
      theme_sparklines
    
    # Plot the data
    ggplotly(ga_plot) %>% layout(autosize=TRUE)
  })
  
  
}

# shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)