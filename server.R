.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(shinyWidgets)


comp_dat_covid_app <- readRDS("comp_dat_covidapp.RDS")

# Data formatting for plot ####
data <- comp_dat_covid_app 
data_active <- filter(comp_dat_covid_app, Active == 'Y')
# Data formatting for table#####
data_table <- comp_dat_covid_app %>%
  mutate(Value = round(Value, 2),
         Variance = round(Variance, 2),
         q25 = round(q25, 2),
         q75 = round(q75, 2)) %>%
  data.frame()
data_table_active <- filter(comp_dat_covid_app, Active == 'Y') %>%
  mutate(Value = round(Value, 2),
         Variance = round(Variance, 2),
         q25 = round(q25, 2),
         q75 = round(q75, 2)) %>%
  data.frame()

## SERVER part of the app.####
# The server piece contains all reactive components.
shinyServer(function(input, output, session) {
  ##Reactive component of the sidebar using renderUI and uiOutput functions####
  ##This allows us to update the sidebar based on other inputs##
  
  # Select type of output
  output$layoutInput <- renderUI({
    radioButtons("layoutInput","Output type", choices = c("Interactive plots", "Data summaries"),
                 selected = "Interactive plots", inline = T)

  })
  
  # select active fisheries
  output$activeInput <- renderUI({
    sliderTextInput("activeInput", "Fisheries active Jan-Mar", choices = c('Active','All fisheries'),
                    selected = 'All fisheries', width = '50%')
  })
  
  # Select levels or Cumulative
  output$cumulInput <- renderUI({
    sliderTextInput("cumulInput", "Cumulative", choices = c('Y', 'N'),
      selected = 'N', width = '25%')
  })
  
  # Select management group
  output$mgrpInput <- renderUI({
    if(input$activeInput == 'All fisheries') {
    selectInput("mgrpInput", "Species groups", choices = unique(data$Species), multiple = T,
                       selected = c('Non-whiting groundfish (IFQ)'))
    } else {
      selectInput("mgrpInput", "Species groups", choices = unique(data_active$Species), multiple = T,
                  selected = c('Non-whiting groundfish (IFQ)'))
    }
  })
  # select state
  output$regionInput <- renderUI({
    if(!input$mgrpInput %in% c('Whiting')) {
    checkboxGroupInput("regionInput", "State", choices = c('All states','California','Oregon','Washington'),
                       selected = c('All states'),
                       inline = T)
    } else {
      checkboxGroupInput("regionInput", "State", choices = c('All states','California','Oregon','Washington', 'At-sea'),
                         selected = c('All states'),
                         inline = T)
    }
  })
  
  # Select a statistic
  output$statInput <- renderUI({
    selectInput("statInput","Statistic", choices = unique(data$Statistic), multiple = F,
                 selected = "Total")
  })
  
  # Select landings in rev or lbs
  output$metricInput <- renderUI({
    selectInput("metricInput", "Landings data", choices = unique(data$Metric), multiple = F,
                 selected = 'Exvessel revenue')
  })

  
  filtered <- reactive({
    if(input$activeInput == 'All fisheries') {
    data %>%
      filter(Species %in% c(input$mgrpInput),
             Statistic == input$statInput,
             Metric == input$metricInput,
             Cumulative == input$cumulInput,
             State %in% c(input$regionInput)
             )
    } else {
      data_active %>%
        filter(Species %in% c(input$mgrpInput),
               Statistic == input$statInput,
               Metric == input$metricInput,
               Cumulative == input$cumulInput,
               State %in% c(input$regionInput)
        )
    }
  })
  
  #creating the dataframe for data table#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered_dt <- reactive({
    if(input$activeInput == 'All fisheries') {
      data_table %>%
        filter(Metric == input$metricInput,
               Statistic == input$statInput,
               Species %in% c(input$mgrpInput),
               Cumulative == input$cumulInput,
               State %in% c(input$regionInput))
    } else {
      data_table_active %>%
        filter(Metric == input$metricInput,
               Statistic == input$statInput,
               Species %in% c(input$mgrpInput),
               Cumulative == input$cumulInput,
               State %in% c(input$regionInput))
    }
  })
  
  dt_dat <- reactive({
    dat <- filtered_dt()
    tabformatfun <- function(x) {
      rounding <- case_when(
        any(dat$Value < 1) ~ 2, 
        all(dat$unit == '') ~ 1, T ~ 0)
      dollar   <- ifelse(grepl('$', dat$ylab, fixed = T), '$', '')
      val = formatC(x, format = 'f', dig = rounding, big.mark = ',')
      return(val)
    }
    dat$Value <-    tabformatfun(dat$Value)
    dat$Variance <- tabformatfun(dat$Variance)
    dat$q25 <-      tabformatfun(dat$q25)
    dat$q75 <-      tabformatfun(dat$q75)
    
    valuetitle <- ifelse(any(dat$Statistic == ''), 'Value', as.character(unique(dat$Statistic)))
    vartitle <- ifelse(input$statInput %in% c('Total', ''), 'Variance',
                       ifelse(input$statInput == 'Median', 'Mean average deviation',
                              'Standard deviation'))
    
    # rename the columns 
    dat <-
      rename(dat,
             !!quo_name(valuetitle)       := Value,
             !!quo_name(vartitle)         := Variance,
             `Quartile: 25th`              = q25,
             `Quartile: 75th`              = q75,
              Month                        = LANDING_MONTH,
              Unit                         = unit)
    
    alwaysexclude <- c('ylab','upper','lower','Type')
    dat <- select(dat, colnames(dat)[apply(dat, 2, function(x) sum(x != '' & x != ' NA' & !is.na(x) & x != 'NA') > 0 )], 
                  -alwaysexclude) 
    
    return(dat)
  })
  
  lineColor <- c(
    '2014-2019' = 'lightgray',
    '2020' = 'blue',
    '35% threshold' = 'red'
  )
  
  # Plot
  output$plot <- renderPlotly({
    if(is.null(filtered())){
      return()
    }  else {
      
          print(
      ggplotly(
    ggplot(filtered(),
           aes(x = LANDING_MONTH,
               y = Value,
               color = Type, 
               group = Year)) +
      scale_color_manual(values = lineColor, name = '') +
      theme_minimal() +
      theme(text = element_text(size = 12),
            axis.text = element_text(size = 8),
            strip.text = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12)) +
      #scale_x_date(date_labels = '%b', date_minor_breaks = "1 month") +
      geom_line(aes(color = Type), size = 0.6) +
      geom_point(aes(color = Type), size = 3) +
      facet_wrap(~ylab, scales = 'free_y', ncol = 2) +
      labs(y = paste(input$statInput, input$metricInput)),
    tooltip = 'Year',
    height = 700,
    width = 800) %>%
    layout(legend = list(orientation = 'h', y = -0.12, x = 0.25),
           margin = list(b = 50, l = 70)
           ))
      
    }
    
  })
  
  output$datsmryPlot <- renderPlot({
  })
  
  # end hover code from stack overflow
  
  ##Creating the data table
  output$table <- DT::renderDT({
    datatable(dt_dat(), 
              rownames = FALSE,
    filter = "top",
      options = list(pageLength = 24)
      )
  })
})