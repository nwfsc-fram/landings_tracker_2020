# set server library here
# For instance: 
.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(shinythemes)
library(shinycssloaders)
library(appFrame)

##UI part of the app. The ui piece is not reactive and is used for setting up the permanent pieces of the app.
# This is where stylesheets can be added

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  appFrameHeaderScrolling(),
  navbarPage(id = "page", collapsible = T, inverse = F,
             title = "Landings Tracker",
             tabPanel("Explore the data", value = "results",
                      h5(em(HTML("This app was designed to be an exploratory tool to look at trends in fisheries landings.<br/> 
                      Depending on the state and the fishery, the lag in data could be as little as a few days or <br/>
                      as long as a month. The data that are likely still incomplete are labeled in the table and <br/>
                      visualized using a blue dotted line. More information about fish ticket completeness <br/>
                      can be found at http://pacfin.psmfc.org/."))),
                      h5(strong("Data were updated on April 29, 2020")),
                      sidebarLayout(
                        sidebarPanel(uiOutput("layoutInput") %>% withSpinner(color="#0dc5c1"),
                                     # only show the sidebar inputs if interactive plots shown #
                                     conditionalPanel(condition = "input.layoutInput == 'Interactive plots'",
                                                      h4(strong("Filtering options")),
                                                      tabsetPanel(type = "pills", id = "filter_ops",
                                                                  tabPanel("Importance",
                                                                           h5(em("Filter fisheries by state and the level of importance (measured as percent revenue)")),
                                                                           uiOutput("state_select"),
                                                                           uiOutput("state_prop")),
                                                                  tabPanel("Seasonality",
                                                                           h5(em("Filter fisheries by month and level of activity (measured as percent revenue)")),
                                                                           uiOutput("month_select"),
                                                                           uiOutput("month_prop")),
                                                                  tabPanel("2020 change",
                                                                           h5(em("Filter fisheries by 2020 percent change compared to 2014-2019")),
                                                                           uiOutput("perc_change")),
                                                                  tabPanel("Custom output",
                                                                           h5(em("Customize output by selecting state and fishery of interest")),
                                                                           uiOutput("regionInput"),
                                                                           uiOutput("mgrpInput"))),
                                                      h4(strong("General settings")),
                                                      uiOutput("metricInput"),
                                                      uiOutput("statInput"),
                                                      uiOutput("cumulInput"),
                                                      uiOutput("wkInput"),
                                                      uiOutput("download_Table"))),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", 
                                               conditionalPanel(condition = "input.layoutInput == 'Interactive plots'",
                                                                plotlyOutput("plot") %>% withSpinner(color="#0dc5c1")),
                                               conditionalPanel(condition = "input.layoutInput == 'Timing plots'",
                                                                img(src = "timing_plot.png", height = '650px', width = '900px'))
                                               ),
                                       # hover = hoverOpts(id ="plot_hover", delay = 50)

                                       # verbatimTextOutput("hover_info"),
                                       # Size parameters don't work with plotly
                                        # height = "1600px"),
                                      tabPanel("Table", 
                                               conditionalPanel(condition = "input.layoutInput == 'Interactive plots'",
                                                                DT::dataTableOutput("table")))
                          )
                        )
                      )),
             tabPanel("Information page",
                       source("info_page.R")$value)
  ),
  appFrameFooterScrolling()
)
)
