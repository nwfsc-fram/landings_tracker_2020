# set server library here
# For instance: 
.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


##UI part of the app. The ui piece is not reactive and is used for setting up the permanent pieces of the app.
# This is where stylesheets can be added

shinyUI(fluidPage(
  navbarPage(id = "page", collapsible = T, inverse = F,
             title = "",
             tabPanel("Explore the data", value = "results",
                      sidebarLayout(
                        sidebarPanel(uiOutput("layoutInput"),
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
                                                                plotlyOutput("plot")),
                                               conditionalPanel(condition = "input.layoutInput == 'Data summaries'",
                                                                plotOutput("datsmryPlot"))
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
  )
)
)
