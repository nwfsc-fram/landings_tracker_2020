.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(shinyWidgets)
#library(fst)
library(dplyr)

comp_dat_covid_app <- readRDS("comp_dat_covidapp.RDS") %>%
  mutate(no_pts = case_when(Type == '2014-2019' ~ 1,
                            Cumulative == 'Y' & Interval == 'Weekly' & Type == '35% threshold' ~ 1,
                            T ~ 0))

# split up the month and other filters befor joining to reduce size of df 
addlfilters <- readRDS("addlfilters.RDS")
month_filter <- select(addlfilters, State, Species, month_prop, select_month)
othr_filter <- select(addlfilters, -c(month_prop, select_month)) %>% distinct()

state_max <- ceiling(max(addlfilters$state_prop))
month_max <- ceiling(max(addlfilters$month_prop))

perc_min <-  floor(min(addlfilters$percchange, na.rm = T))
perc_max <- ceiling(max(addlfilters$percchange, na.rm = T))

# Data formatting for plot ####
# data with month filter
data_m <- comp_dat_covid_app %>%
  left_join(month_filter) %>%
  data.frame()

# data with other filters
data <- comp_dat_covid_app %>%
  left_join(othr_filter) %>%
  data.frame()

# Data formatting for table#####
# data with month filter
data_table_m <- data_m %>%
  mutate(Value = round(Value, 2),
         Variance = round(Variance, 2),
         q25 = round(q25, 2),
         q75 = round(q75, 2)) %>%
  data.frame()
# data with other filter
data_table <- data %>%
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
  
  # Input that applies to all ####
  # Select type of output
  output$layoutInput <- renderUI({
    radioButtons("layoutInput","Output type", choices = c("Interactive plots", "Data summaries"),
                 selected = "Interactive plots", inline = T)
  })
  # Select levels or Cumulative
  output$cumulInput <- renderUI({
    sliderTextInput("cumulInput", "Cumulative", choices = c('Y', 'N'),
      selected = 'N', width = '25%')
  })
  # Select weekly or monthly
  output$wkInput <- renderUI({
    sliderTextInput("wkInput", "Time interval", choices = c('Weekly', 'Monthly'),
                    selected = 'Monthly', width = '25%')
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
  # Download button#####
  output$download_Table <- renderUI({
    tags$div(class = "actbutton",
             downloadButton("dlTable", "Download Data Table", class = "btn btn-info"))
  })
  
  # Input that applies to "custom input" ####
  # Select management group
  output$mgrpInput <- renderUI({
    selectInput("mgrpInput", "Species groups", choices = unique(data$Species), multiple = T,
                       selected = c('Non-whiting groundfish (IFQ)'))
  })
  # select state
  output$regionInput <- renderUI({
    if(!'Whiting' %in% input$mgrpInput) {
    checkboxGroupInput("regionInput", "State", choices = c('All states','California','Oregon','Washington'),
                       selected = c('All states'),
                       inline = T)
    } else {
      checkboxGroupInput("regionInput", "State", choices = c('All states','California','Oregon','Washington', 'At-sea'),
                         selected = c('All states'),
                         inline = T)
    }
  })
  
  # Input that applies to "importance" ####
  # select proportion of revenue by state
  output$state_select <- renderUI({
      checkboxGroupInput("state_select", "", choices = unique(data$State),
                         selected = c('Oregon'),
                         inline = T)
  })
  output$state_prop <- renderUI({
    sliderInput("state_prop", label = "",
                min = 0, max = state_max, value = c(10, state_max), step = 10)
  })
  
  
  # Input that applies to seasonality ####
  # Filter by proportion of revenue by month
  output$month_select <- renderUI({
    selectInput("month_select", "", choices = unique(data_m$select_month),
                multiple = F, selected = 'May')
  })
  
  output$month_prop <- renderUI({
    sliderInput("month_prop", label = "",
                min = 0, max = month_max, value = c(20,month_max), step = 10)
  })
  
  # Input that applies to 2020 change ####
  output$perc_change <- renderUI({
    sliderInput("perc_change", label = "",
                min = perc_min, max = perc_max, value = c(-20, 0), step = 20)
  })
# Reactive Data component ####
  filtered <- reactive({
    if(input$filter_ops == "Importance") {
    data %>%
      subset(Statistic == input$statInput &
             Metric == input$metricInput &
             Cumulative == input$cumulInput &
             Interval == input$wkInput &
             State %in% c(input$state_select) &
             state_prop >= input$state_prop[1] &
             state_prop <= input$state_prop[2]
             ) %>%
        data.frame()
    }
    else if(input$filter_ops == "Seasonality") {
      data_m %>%
        subset(Statistic == input$statInput &
          Metric == input$metricInput &
          Cumulative == input$cumulInput &
          Interval == input$wkInput &
          # get rid of the "All states"
          State != 'All states' & 
          select_month == input$month_select &
          month_prop >= input$month_prop[1] &
          month_prop <= input$month_prop[2]
        )
    }
    else if(input$filter_ops == '2020 change') {
      data %>%
        subset(Statistic == input$statInput &
                 Metric == input$metricInput &
                 Cumulative == input$cumulInput &
                 Interval == input$wkInput &
                 # get rid of the "All states"
                 State != 'All states' & 
                 percchange >= input$perc_change[1] &
                 percchange <= input$perc_change[2]
        )
    }
    else if(input$filter_ops == "Custom output") {
      data %>%
        subset(Species %in% c(input$mgrpInput) &
          Statistic == input$statInput &
          Metric == input$metricInput &
          Cumulative == input$cumulInput &
          Interval == input$wkInput &
          State %in% c(input$regionInput)
        )
    }
  })
  
  #creating the dataframe for data table#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered_dt <- reactive({
    if(input$filter_ops == "Importance") {
      data_table %>%
        subset(Statistic == input$statInput &
                 Metric == input$metricInput &
                 Cumulative == input$cumulInput &
                 Interval == input$wkInput &
                 State %in% c(input$state_select) &
                 state_prop >= input$state_prop[1] &
                 state_prop <= input$state_prop[2]
        ) %>%
        data.frame()
    }
    else if(input$filter_ops == "Seasonality") {
      data_table_m %>%
        subset(Statistic == input$statInput &
                 Metric == input$metricInput &
                 Cumulative == input$cumulInput &
                 Interval == input$wkInput &
                 # get rid of the "All states"
                 State != 'All states' & 
                 select_month == input$month_select &
                 month_prop >= input$month_prop[1] &
                 month_prop <= input$month_prop[2]
        )
    }
    else if(input$filter_ops == '2020 change') {
      data_table %>%
        subset(Statistic == input$statInput &
                 Metric == input$metricInput &
                 Cumulative == input$cumulInput &
                 Interval == input$wkInput &
                 # get rid of the "All states"
                 State != 'All states' & 
                 percchange >= input$perc_change[1] &
                 percchange <= input$perc_change[2]
        )
    }
    else if(input$filter_ops == "Custom output") {
      data_table %>%
        subset(Species %in% c(input$mgrpInput) &
                 Statistic == input$statInput &
                 Metric == input$metricInput &
                 Cumulative == input$cumulInput &
                 Interval == input$wkInput &
                 State %in% c(input$regionInput)
        )
    }
  })
  
  dt_dat <- reactive({
    if(is.null(filtered_dt())){
      return()
    }
    if(input$wkInput == 'Monthly') {
    dat <- filtered_dt() %>%
      mutate(Date = format(LANDING_MONTH, "%B"))
    } else {
      dat <- filtered_dt()
    }
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
              Date                        = Date,
              Unit                         = unit)
    
    alwaysexclude <- c('ylab','upper','lower','Type', 'LANDING_MONTH', 'no_pts',
                       'Cumulative','Interval')
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
    } else {
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
      scale_x_date(date_labels = '%b', date_breaks = "1 month") +
      geom_line(data = filter(filtered(), Type == '2014-2019'), size = 0.6,
                mapping = aes(color = Type)) +
      geom_line(data = filter(filtered(), !is.na(Value) & Type != '2014-2019'), linetype = 'dotted') +
      geom_line(data = filter(filtered(), Type != '2014-2019'),
                mapping = aes(color = Type), size = 0.6) +
      geom_point(data = filter(filtered(), no_pts == 0),
                 mapping = aes(color = Type), size = 1.5) +
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
  
  output$dlTable <- downloadHandler(
    filename = function() { 'landings_tracker_2020.csv' },
    content = function(file) {
      table <- filtered_dt()
      row.names(table) <- NULL
      table$source <- ""
      names(table)[names(table) == 'source'] <- "Sourced from the Landings Tracker 2020 application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/landings_tracker/) maintained by NOAA Fisheriess NWFSC"
      write.csv(table, file)
    })
})