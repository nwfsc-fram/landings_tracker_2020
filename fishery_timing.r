# This script pulls data and formats the dataframe for use in the app #
library(dplyr)
library(reshape2)
library(lubridate)
library(EDCReport)
library(tidyr)
library(ggplot2)
library(viridis)

comp_dat_covid_app <- readRDS("comp_dat_covidapp.RDS") 

dat <- subset(comp_dat_covid_app,
  Statistic == 'Total' &
    Metric == 'Exvessel revenue' & 
    Cumulative == 'N' &
    Interval == 'Monthly' &
    Type == '2014-2019')

dat_summ <- group_by(dat, State, Species, ylab, LANDING_MONTH) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  group_by(State, Species, ylab) %>%
  mutate(prop = Value/sum(Value, na.rm = T)) %>%
  mutate(date_end = as.Date(LANDING_MONTH)+
      months(1)) %>%
  ungroup() %>%
  mutate(ylab = paste0(State, ": ", Species))

subset(dat_summ, grepl('California', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, #size = prop, 
    color = prop), size = 2) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('')

subset(dat_summ, grepl('Oregon', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, #size = prop, 
    color = prop), size = 2) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('')

subset(dat_summ, grepl('Washington', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, #size = prop, 
    color = prop), size = 2) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('')
  
