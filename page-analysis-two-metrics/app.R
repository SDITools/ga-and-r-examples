# Load the necessary libraries. 
library(shiny)
library(googleAuthR)       # For authentication
library(googleAnalyticsR)  # How we actually get the Google Analytics data

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")
options(googleAuthR.redirect = "https://gilligan.shinyapps.io/page-analysis/")

library(tidyverse)         # Includes dplyr, ggplot2, and others; very key!
library(knitr)             # Nicer looking tables
library(plotly)            # We're going to make the charts interactive
library(DT)                # Interactive tables
library(scales)            # Useful for some number formatting in the visualizations

# Create a date frame that we'll use to look up the various calculations for different
# metrics.
calcs_df <- data.frame(metric = c("Entrances", "Bounces", "Unique Pageviews", "Pageviews", "Exits", "Bounce Rate", "Exit Rate"),
                       calculation = c("entrances", "bounces", "uniquePageviews", "pageviews", "exits", "bounces/entrances", "exits/pageviews"),
                       metric_format = c("integer", "integer", "integer", "integer", "integer", "percentage", "percentage"),
                       stringsAsFactors = FALSE)

# Define the base theme for visualizations
theme_base <- theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "gray70"),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(colour = "gray20", face = "bold"))

## ui.R
ui <- fluidPage(title = "Page Analysis of Two Metrics",
                tags$head(includeScript("gtm.js")),
                tags$h2("Page Analysis of Two Metrics*"),
                tags$div(paste("Select a Google Analytics view and date range and then pull the data. From there, explore combinations",
                      "of metrics. Depending on the metrics you choose, the pages of interest will be in specific quadrants as",
                      "opportunity pages or potential problem pages.")),
                tags$br(),
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
                               # Page filter (regEx)
                               textInput("filter_regex",
                                         label = "Enter regEx to filter to the pages of interest:",
                                         value = ".*"),
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
                               tags$h4("Explore the Data!"),
                               selectInput("x_dim",
                                           label = "Metric for the x-axis:",
                                           choices = calcs_df$metric,
                                           selected = "Entrances"),
                               selectInput("x_scale",
                                           label = "Scale for the x-axis:",
                                           choices = c("Linear", "Logarithmic"),
                                           selected = "Linear"),
                               selectInput("y_dim",
                                           label = "Metric for the y-axis:",
                                           choices = calcs_df$metric,
                                           selected = "Bounce Rate"),
                               selectInput("y_scale",
                                           label = "Scale for the y-axis:",
                                           choices = c("Linear", "Logarithmic"),
                                           selected = "Linear"),
                               sliderInput("num_pages",
                                           label = "# of pages to include (by X metric):",
                                           min = 10,
                                           max = 200,
                                           value = 50,
                                           step = 5)),
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel("Overall",
                                                 tags$br(),
                                                 tags$h4(textOutput("corr")),
                                                 tags$hr(),
                                                 plotlyOutput("plot_overall", height = "800px")),
                                        tabPanel("Channel Grouping and Device Category",
                                                 plotlyOutput("plot_facets", height = "800px")))
                  )
                ),
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
  get_ga_data <- reactive({
    
    # Only pull the data if the "Get Data" button is clicked
    input$query_data
    
    # Pull the data. 
    isolate(google_analytics(viewId = view_id(),
                     date_range = input$date_selection,
                     metrics = c("entrances", "bounces", "pageviews", 
                                 "uniquePageviews", "exits"),
                     dimensions = c("deviceCategory", "channelGrouping", "pagePath"),
                     dim_filters = page_filter(),
                     anti_sample = input$anti_sampling))
  })
  
  # Reactive function to get the formulas used to calculate the x metric
  get_formula_x <- reactive({
    calc_details_x <- calcs_df %>%  filter(metric == input$x_dim)
    formula_x <- calc_details_x$calculation
  })
  
  # Reactive function to get the formulas used to calculate the y metric
  get_formula_y <- reactive({
    calc_details_y <- calcs_df %>%  filter(metric == input$y_dim)
    formula_y <- calc_details_y$calculation
  })
  
  # Reactive function to get the plottable data. This includes calculating the metrics
  # for x and y dimensions This last step requires NSE,# which, I realize, has moved on beyond 
  # the "_" notation, but I'll be damned if I could get that to work.
  get_data_overall <- reactive({
    
    # Get the data
    data_pages <- get_ga_data()
    
    # Get the formulas
    formula_x <- get_formula_x()
    formula_y <- get_formula_y()
    
    data_overall <- data_pages %>%  group_by(pagePath) %>% 
      summarise(entrances = sum(entrances), bounces = sum(bounces), pageviews = sum(pageviews),
                uniquePageviews = sum(uniquePageviews), exits = sum(exits)) %>% 
      mutate_(x = formula_x,
              y = formula_y)
  })
  
  # Reactive function to get the top pages by the x-axis value. We'll use this for our overall plot
  get_top_pages <- reactive({
    
    # Get the overall data (with x and y values calculated)
    data_overall <- get_data_overall()
    
    # Filter down to the num_pages pages
    top_pages <- data_overall %>% 
      arrange(-x) %>% 
      top_n(input$num_pages, x) %>% 
      dplyr::select(pagePath, x, y)
  })
  
  # Reactive function to get the top pages by the x-axis value, but to keep that data with
  # the device category and channel grouping breakouts. This will be used for the faceted plot.
  get_top_pages_faceted <- reactive({

    # Get the overall data
    data_pages <- get_ga_data()
    
    # Get the top pages overall
    top_pages <- get_top_pages()
    
    # Get the formulas
    formula_x <- get_formula_x()
    formula_y <- get_formula_y()
    
    # Stitch that back to the broken-out (facetable) data
    top_pages_faceted <- top_pages %>% 
      dplyr::select(pagePath) %>% 
      left_join(data_pages) %>% 
      mutate_(x = formula_x,  y = formula_y) %>% 
      dplyr::select(deviceCategory, channelGrouping, pagePath, x, y)
  })

  
  # Output the correlation coefficient and R-squared
  output$corr <- renderText({
    
    # Get the top pages
    top_pages <- get_top_pages()
    
    # Calculate the correlation coefficient (r) and the coefficient of determination (r^2)
    r <- round(cor(top_pages$x, top_pages$y),2)
    r_squared <- round(cor(top_pages$x, top_pages$y)^2,2)
    
    # Output the result
    paste0("These two metrics have a correlation coefficient of ", r, " and an R-squared of ", r_squared, ".")
  })
  
  # We've got to do a lot of fiddling with the scales for both plots, so we're going
  # to put in a reactive function that figures all of that out and returns a list with
  # all of the various bits
  get_axis_settings <- reactive({
    
    # Grab the format for x and y from the data frame
    x_format <- filter(calcs_df, metric == input$x_dim) %>% dplyr::select(metric_format) %>% as.character()
    y_format <- filter(calcs_df, metric == input$y_dim) %>% dplyr::select(metric_format) %>% as.character()
    
    # Set up the x and y scales. These vary based on the settings in the initial chunk
    format_x <- if(x_format == "integer"){comma} else {percent_format(accuracy = 1)}
    format_y <- if(y_format == "integer"){comma} else {percent_format(accuracy = 1)}
    
    # Get the plot scale (linear or log plus the format) for the axes
    if(input$x_scale == "Linear"){
      scale_x <- scale_x_continuous(labels = format_x)
    } else {
      scale_x <- scale_x_log10(labels = format_x)
    }
    
    if(input$y_scale == "Linear"){
      scale_y <- scale_y_continuous(labels = format_y)
    } else {
      scale_y <- scale_y_log10(labels = format_y)
    }
    
    # Get the (loose) quadrant divider hline and vline locations. These are in different spots
    # for the overall and for the faceted version (because the overall is rolled up totals -- which
    # means larger max #s)
    
    # Grab the top pages overall (for overall max/min) and the detailed pages (for the faceted max/min)
    top_pages <- get_top_pages()
    top_pages_faceted <- get_top_pages_faceted()
    
    # Set the vline values for the x-axis on both plots
    if(input$x_scale == "Linear" | x_format == "percentage"){
      x_vline_overall <- max(top_pages$x, na.rm=TRUE)/2
      x_vline_faceted <- max(top_pages_faceted$x, na.rm=TRUE)/2
    } else {
      x_vline_overall <- max(top_pages$x, na.rm=TRUE) %>% sqrt()
      x_vline_faceted <- max(top_pages_faceted$x, na.rm=TRUE) %>% sqrt()
    }
    
    # Set the hline values for the y-axis on both plots
    if(input$y_scale == "Linear" | y_format == "percentage"){
      y_hline_overall <- max(top_pages$y, na.rm=TRUE)/2
      y_hline_faceted <- max(top_pages_faceted$y, na.rm=TRUE)/2
    } else {
      y_hline_overall <- max(top_pages$y, na.rm=TRUE) %>% sqrt()
      y_hline_faceted <- max(top_pages_faceted$y, na.rm=TRUE) %>% sqrt()
    }
    
    # Pack all of this up into a list to return
    axis_settings <- list(format_x = format_x,
                          format_y = format_y,
                          scale_x = scale_x,
                          scale_y = scale_y,
                          x_vline_overall = x_vline_overall,
                          x_vline_faceted = x_vline_faceted,
                          y_hline_overall = y_hline_overall,
                          y_hline_faceted = y_hline_faceted)
  })
  
  # Output the overall plot (interactively)
  output$plot_overall <- renderPlotly({
    
    # Get the top pages
    top_pages <- get_top_pages()
    
    # Get the axis settings
    axis_settings <- get_axis_settings()
    
    # Build the plot
    gg <- ggplot(top_pages, mapping = aes(x = x, y = y, text = pagePath)) +
      axis_settings$scale_x +
      geom_vline(xintercept = axis_settings$x_vline_overall, colour = "gray90") +   # vertical line quadrant divider
      axis_settings$scale_y +
      geom_hline(yintercept = axis_settings$y_hline_overall, colour = "gray90") +   # horizontal line quadrant divider
      geom_point(colour = "steelblue", alpha = 0.8) +
      labs(title = paste0("Page Analysis: Top ", input$num_pages, " Pages by Total ", input$x_dim, ": ", 
                          format(input$date_selection[1]), " to ", format(input$date_selection[2])),
           x = input$x_dim, y = input$y_dim) +
      theme_base
    
    ggplotly(gg) %>% layout(autosize=TRUE)
    
  })
  
  # Output the plot broken down by device category and channel grouping
  output$plot_facets <- renderPlotly({
    
    # Get the top page - faceteds
    top_pages_faceted <- get_top_pages_faceted()
    
    # Get the axis settings
    axis_settings <- get_axis_settings()
    
    gg <- ggplot(top_pages_faceted, mapping = aes(x = x, y = y, text = pagePath)) +
      axis_settings$scale_x +
      geom_vline(xintercept = axis_settings$x_vline_faceted, colour = "gray90") +   # vertical line quadrant divider
      axis_settings$scale_y +
      geom_hline(yintercept = axis_settings$y_hline_faceted, colour = "gray90") +   # horizontal line quadrant divider
      geom_point(colour = "steelblue", alpha = 0.8) +
      labs(title = paste0("Page Analysis: Top ", input$num_pages, " Pages by Total ", input$x_dim, ": ", 
                          format(input$date_selection[1]), " to ", format(input$date_selection[2])),
           x = input$x_dim, y = input$y_dim) +
      facet_grid(channelGrouping ~ deviceCategory, switch = "y") +
      theme_base +
      theme(panel.border = element_rect(colour = "gray50", fill = NA))

    ggplotly(gg) %>% layout(autosize=TRUE)
    
  })
  
}

# shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
