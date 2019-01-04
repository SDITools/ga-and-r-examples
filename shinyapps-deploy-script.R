# Use this script -- or some tweaked version of it -- to push the code
# to shinyapps.io. Comment out the deployApp() call that you do NOT 
# want to run. This script requires having rsconnect installed, as well
# as having authenticated with a shinyapps.io 

library(rsconnect)

# Deploy time-normalized page analyzer. It's a bit clunky, in that we seem to need
# to manually set the working directory to be the directory where the Shiny app and
# supporting files exist before deploying. Because this R project has multiple 
# Shiny apps, and the app file name has to be app.R, each app is in its own directory.
setwd("time-normalized-pageviews")
deployApp(appFiles = c("app.R", "ga-web-client.json"),
          appName = "time-normalized",
          appTitle = "Google Analytics - Time-Normalized Pageviews")
setwd("..")


