library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)

source('confTreat.r')
source('helperfns.r')
comp_dat_all <- readRDS("comp_dat_all.RDS") 


dat <- subset(comp_dat_all,
  YEAR %in% 2014:2019 & 
  Metric == 'EXVESSEL_REVENUE' &
  !grepl('SHELLFISH', SPECIES_GROUP))

confidentiality <- confTreat(dat, c('SPECIES_GROUP','LANDING_MONTH','AGENCY_CODE'),
           valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'), whichtables = 'flagtable')

dat_summpreconf <- group_by(dat, AGENCY_CODE, SPECIES_GROUP, LANDING_MONTH) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  group_by(AGENCY_CODE, SPECIES_GROUP) %>%
  mutate(prop = Value/sum(Value, na.rm = T)) 

dat_summconf <- left_join(dat_summpreconf, confidentiality)


dat_summ <- mutate(
  dat_summconf,
  Species = convert_sp(SPECIES_GROUP),
  State   = convert_state(AGENCY_CODE),
  ylab = paste0(State, ": ", Species)
) %>%
  mutate(LANDING_MONTH = ymd(paste0('2001-', LANDING_MONTH, '-01'))) %>%
  mutate(date_end = as.Date(LANDING_MONTH) +
      months(1))



subset(dat_summ, grepl('California', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, 
    color = prop, size = prop)) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('') +
  geom_segment(data = filter(dat_summ, prop == 0 & grepl('California', ylab)), mapping = aes(
    x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab), 
    color = 'black') +
  ggtitle('Proportion of total annual revenue (2014-2019)')

subset(dat_summ, grepl('Oregon', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, size = prop, 
    color = prop)) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('') +
  geom_segment(data = filter(dat_summ, prop == 0 & grepl('Oregon', ylab)), mapping = aes(
    x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab), 
    color = 'black') +
  ggtitle('Proportion of total annual revenue (2014-2019)')

subset(dat_summ, grepl('Washington', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = ylab)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, size = prop, 
    color = prop)) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('') +
  geom_segment(data = filter(dat_summ, prop == 0 & grepl('Washington', ylab)), mapping = aes(
    x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab), 
    color = 'black') +
  ggtitle('Proportion of total annual revenue (2014-2019)')
  
subset(dat_summ, grepl('Whiting', ylab)) %>%
ggplot(aes(x = LANDING_MONTH, y = State)) +
  geom_segment(aes(x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab, size = prop, 
    color = prop)) +
  scale_color_viridis() +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  ylab('') +
  xlab('') +
  geom_segment(data = filter(dat_summ, prop == 0 & grepl('Washington', ylab)), mapping = aes(
    x = LANDING_MONTH, y = ylab, 
    xend = date_end, yend = ylab), 
    color = 'black') +
  ggtitle('Proportion of total annual revenue (2014-2019)')
  