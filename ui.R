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
                                     conditionalPanel(condition = "input.layoutInput == 'Interactive plots'",
                                                      uiOutput("metricInput"),
                                                      uiOutput("regionInput"),
                                                      uiOutput("mgrpInput"),
                                                      uiOutput("statInput"),
                                                      uiOutput("cumulInput"),
                                                      uiOutput("activeInput"))
                          ),
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
                      )#,
             # tabPanel("Information page",
             #          source("info_page.R")$value)
  )
)
))
