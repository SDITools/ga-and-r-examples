# Google Analytics (and a little bit of Twitter) and R

This repository has 5 (4.25, really) examples of using R with Google Analytics. These examples are designed to work with the free version of Google Analytics with just the base tag (implemented). This makes the examples less impactful, but the idea is that it makes them a safe and easy jumping off point for anyone who wants to use them.

One set of slides that walks through the ideas and concepts behind these examples is available in [Google Slides](https://docs.google.com/presentation/d/1UBtbuIPmZ6yUj5VLPNYDcbV_0JSZzyyugvDPcAJd8Fs/edit?usp=sharing) or as slides with audio voiceover [on YouTube](https://youtu.be/SRZPLIcCvFU).

## There are Two Versions of Each Example
There are two versions of each example here:

* **The R Notebook version** -- the RMarkdown version that has pretty extensively annotated code and explanations. This is the code you can use to tinker with and just run locally within RStudio
* **The `Shiny` version (`app.R`)** -- this is the web-enabled experience that, if published to a Shiny server (including shinyapps.io) can be used by anyone to log in to their own account and explore the example _without_ seeing/experiencing any of the code. 

Links to "executed code showing output" (**RPubs**) as well as links to Shiny apps (**Shiny**) that can be tried out with no coding are included in the table below.

| Example | Folder Name | Examples |
|-----------------------------------------------------|---------------|:-------------:|
| Time-Normalized Pageviews | `time-normalized-pageviews` | [RPubs](http://rpubs.com/tgwilson/time-normalized-pageviews) / [Shiny](https://gilligan.shinyapps.io/time-normalized/) |
| Page-Level Analysis/Comparison with Two Metrics | `page-analysis-two-metrics` | [RPubs](http://rpubs.com/tgwilson/page-analysis) / [Shiny](https://gilligan.shinyapps.io/page-analysis/) |
| (Light) Text Mining of On-Site Search Data | `site-search-analysis` | [RPubs](http://rpubs.com/tgwilson/day-of-week-regression) / [Shiny](https://gilligan.shinyapps.io/site-search/) |
| (Light) Text Mining of Twitter Followers | `twitter-followers` | [RPubs](http://rpubs.com/tgwilson/twitter-followers) |
| Regression Analysis with a Nominal / Categorical Value (Day of Week) | `regression` | [RPubs](http://rpubs.com/tgwilson/site-search) / [Shiny](https://gilligan.shinyapps.io/regression/) |

## A Few Notes on the Shiny Apps

The Shiny apps listed above should work fairly well, although they have extremely limited error handling. So, if you're using them and run into an error, please do log an issue in this repository.

If you are interested in using the Shiny code itself and modifying the code for your own purposes, there are a few things to be aware of:

* You need to set up "Web service" credentials for a Google app that has Google Analytics API access
* Download the JSON file for those credentials
* Place that JSON file either in the same folder as `app.R` or somewhere accessible and then add `"GAR_CLIENT_WEB_JSON="[path to that file]"` to the `.Renviron` file that will be referenced by that app
* Basically, see the last post in [this issue](https://github.com/MarkEdmondson1234/googleAuthR/issues/136) for details.
* The apps include Google Analytics tracking in them. IF you want to do that as well, then you need to get the JavaScript (recommended is to use Google Tag Manager) and place it in an include file (`gtm.js`) in the same folder as the app. If you do not want to track activity on your app using Google Analytics, then simply remove this line of code from the app: `tags$head(includeScript("gtm.js")),`.
