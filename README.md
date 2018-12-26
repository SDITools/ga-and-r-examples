# ga-and-r-examples
Examples using Google Analytics with R for a range of specific use cases

# Two Versions of Each Example
There are two versions of each example here:

* **The R Notebook version** -- the RMarkdown version that has pretty extensively annotated code and explanations. This is the code you can use to tinker with and just run locally within RStudio
* **The `...-shiny.R` version** -- the Shiny version; this is the web-enabled experience that, if published to a Shiny server (including shinyapps.io) can be used by anyone to log in to their own account and explore the example _without_ seeing/experiencing any of the code.

# For Shiny

Various notes:
* You need to set up "Web service" credentials
* Download the JSON file for those credentials
* Place that JSON file somewhere accessible and then add `"GAR_CLIENT_WEB_JSON="[path to that file]"` the the `.Renviron` file that will be referenced by that app.
