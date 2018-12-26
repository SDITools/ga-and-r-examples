# Load the necessary libraries. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               googleAuthR)

# This needs to be called before googleAnalyticsR is loaded so that googleAnalyticsR
# will use project specified in the JSON credentials loaded as part of this call
gar_set_client(scopes = c("https://www.googleapis.com/auth/analytics.readonly"))

# Load the other libraries we'll use.
pacman::p_load(googleAnalyticsR,  # How we actually get the Google Analytics data
               tidyverse,         # Includes dplyr, ggplot2, and others; very key!
               knitr,             # Nicer looking tables
               plotly,            # We're going to make the charts interactive
               scales)            # Useful for some number formatting in the visualizations

## ui.R
ui <- fluidPage(title = "Time-Normalized Pageviews",
                tags$h2("Time-Normalized Pageviews"),
                sidebarLayout(
                  sidebarPanel("This is the sidebar.",
                               authDropdownUI("auth_menu",
                                              inColumns = FALSE)),
                  mainPanel("This is the main panel.")
                )
)

## server.R
server <- function(input, output, session){
  
  # Create a non-reactive access token
  gar_shiny_auth(session)
  
  # Populate the Account/Property/View dropdowns and return whatever the
  # selected view ID is
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account_list)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
