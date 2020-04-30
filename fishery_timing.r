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

sswhit_allstates <- subset(dat, AGENCY_CODE %in% c('W', 'C', 'O') & SPECIES_GROUP == 'WHITING') %>%
  mutate(AGENCY_CODE = 'All') %>%
  mutate(SPECIES_GROUP = 'Shoreside whiting')

aswhit_allstates <- subset(dat, AGENCY_CODE %in% c('F') & SPECIES_GROUP == 'WHITING') %>%
  mutate(AGENCY_CODE = 'All') %>%
  mutate(SPECIES_GROUP = 'At-sea whiting')

dat_split_whiting <- subset(dat, !(SPECIES_GROUP == 'WHITING' & AGENCY_CODE %in% c('F', 'All'))) %>%
  bind_rows(sswhit_allstates) %>%
  bind_rows(aswhit_allstates) %>%
  mutate(SPECIES_GROUP = ifelse(SPECIES_GROUP == 'WHITING', 'Shoreside whiting', SPECIES_GROUP))

confidentiality <- confTreat(dat_split_whiting, c('SPECIES_GROUP','LANDING_MONTH','AGENCY_CODE'),
           valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'), whichtables = 'flagtable')

dat_summpreconf <- group_by(dat_split_whiting, AGENCY_CODE, SPECIES_GROUP, LANDING_MONTH) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  group_by(AGENCY_CODE, SPECIES_GROUP) %>%
  mutate(prop = Value/sum(Value, na.rm = T)) 

dat_summconf <- left_join(dat_summpreconf, confidentiality) %>%
  mutate(prop = ifelse(final == 'suppress', NA, prop))


dat_summ <- mutate(dat_summconf,
  Species = case_when(SPECIES_GROUP %in% c('Shoreside whiting', 'At-sea whiting') ~ SPECIES_GROUP, T ~ convert_sp(SPECIES_GROUP)),
  State   = convert_state(AGENCY_CODE),
  ylab = paste0(State, ": ", Species)
) %>%
  mutate(LANDING_MONTH = ymd(paste0('2001-', LANDING_MONTH, '-01'))) %>%
  mutate(date_end = as.Date(LANDING_MONTH) +
      months(1))

dat_summ_rev <- dat_summ %>%
  group_by(State, Species) %>%
  summarize(Revenue = median(Value)/1e6) %>%
  mutate(LANDING_MONTH = as.Date('2002-03-01'),
         Revenue = case_when(Revenue < 0.01 ~ "< 0.01",
                             T ~ as.character(round(Revenue, 2))))

dat_summ_final <- full_join(dat_summ, dat_summ_rev) %>%
  mutate(Species2 = factor(Species, levels = rev(c(
  'Dungeness crab'                                    ,
  'Shrimp'                                            ,
  'At-sea whiting'                                    ,
  'Shoreside whiting'                                 ,
  'Non-whiting groundfish (IFQ; midwater trawl)'      ,
  'Non-whiting groundfish (IFQ; bottom trawl)'        ,
  'Non-whiting groundfish (Fixed gear; nearshore)'    ,
  'Non-whiting groundfish (Fixed gear; non-nearshore)',
  'Non-whiting groundfish (Fixed gear; other)'        ,
  'Anchovy'                                           ,
  'Market squid'                                      ,
  'Puget Sound fisheries'                             ,
  'Salmon'                                            ,
  'Sardine'                                           ,
  'Tuna'                                              ,
  'Other coastal pelagic'                             ,
  'Other crab'                                        ,
  'Other species'
))))
# Facet plot,
timing_plots <- filter(dat_summ_final#, State != 'All states'
  ) %>%
  mutate(rev_lab = ifelse(is.na(Revenue), NA, paste0("$", Revenue, "M"))) %>%
  mutate(prop_bin = case_when(prop < .1 ~ '<.1',
    prop <= .1 & prop < .2 ~ '.1-.2',
    prop <= .2 & prop < .3 ~ '.2-.3',
    prop <= .3 & prop < .4 ~ '.3-.4',
    T ~ '>.5')) %>%
  ggplot(aes(x = LANDING_MONTH, y = Species2, group = prop)) +
  geom_segment(aes(x = LANDING_MONTH, y = Species2, 
                   xend = date_end, yend = Species2, size = prop, 
                   color = prop)) +
  scale_color_viridis(name = 'Proportion of\nlandings revenue') +
  guides(size = F) +
  scale_x_date(date_labels = '%b', date_breaks = "1 month") +
  geom_segment(data = filter(dat_summ, prop == 0), mapping = aes(
    x = LANDING_MONTH, y = Species, 
    xend = date_end, yend = Species), 
    color = 'black') +
  geom_text(aes(label = rev_lab), nudge_x=.7, check_overlap = T, size = 3, hjust = 'right') +
  facet_wrap(~State) +
  labs(title = 'Proportion of total annual ex-vessel revenue (2014-2019)',
       subtitle = "Text labels: Median annual ex-vessel revenue in millions",
       x = "",
       y = "")
ggsave('www/timing_plot.png', timing_plots, width = 13, height = 9, units = 'in')
  
ca_timing <- subset(dat_summ, grepl('California', ylab)) %>%
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
  ggtitle('California fisheries: Proportion of total annual revenue (2014-2019)')
ggsave('www/ca_timing.png', ca_timing, width = 11, height = 5, units = 'in')


or_timing <- subset(dat_summ, grepl('Oregon', ylab)) %>%
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
  ggtitle('Oregon fisheries: Proportion of total annual revenue (2014-2019)')
ggsave('www/or_timing.png', or_timing, width = 11, height = 5, units = 'in')

wa_timing <- subset(dat_summ, grepl('Washington', ylab)) %>%
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
  ggtitle('Washington fisheries: Proportion of total annual revenue (2014-2019)')
ggsave('www/wa_timing.png', wa_timing, width = 11, height = 5, units = 'in')
  
whiting_timing <- subset(dat_summ, grepl('Whiting', ylab)) %>%
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
ggsave('whiting_timing.png', whiting_timing, width = 11, height = 5, units = 'in')