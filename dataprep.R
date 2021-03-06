# This script pulls data and formats the dataframe for use in the app #
library(dplyr)
library(reshape2)
library(lubridate)
library(EDCReport)
library(tidyr)
#library(fst)

source("confTreat.R")
source("helperfns.R")

# Set completeness cutoff at 14 days prior to today. All data after this date will be shown with uncertainty
# For Washington apply 28 day cutoff.
today <- Sys.Date()
completeness_cutoff <- today - 14
month_cutoff <- month(completeness_cutoff) - 1
# cutoffs for washington
wk4_completeness <- today - 28
wk4_month <- month(wk4_completeness) - 1

# Deflator #####
# Currently coded to treat 2020 as 2019$
defl <- structure(list(YEAR = c(2014, 2015, 2016, 2017, 2018, 2019), 
    DEFL = c(0.92239, 0.93203, 0.94167, 0.95938, 0.98276, 1)), row.names = 30:35, class = "data.frame")
defl2020 <- filter(defl, YEAR == 2019) %>%
  mutate(YEAR = 2020)
defl_adj <- rbind(defl, defl2020)

# species groups changes
wa_othr <- c('OTHER CRAB', 'ANCHOVY', 'SARDINE', 'OTHER COASTAL PELAGIC')

# Load data from data_pull.R ####
comp_dat_raw1 <- readRDS('comp_dat_raw.RDS') %>%
  rename(YEAR = LANDING_YEAR) %>%
  select(-TICKET_SOURCE_CODE) %>%
  subset(!(AGENCY_CODE == 'C' & SPECIES_GROUP == 'WHITING')) %>%
  # remove shellfish
  subset(SPECIES_GROUP != 'SHELLFISH') %>%
  # move wa/or cps into other species
  mutate(SPECIES_GROUP = case_when(AGENCY_CODE %in% c('W', 'O') & SPECIES_GROUP %in% wa_othr ~ 'OTHER',
                                   T ~ SPECIES_GROUP))

# Creat an "all non-whiting groundfish" category
comp_dat_raw <- comp_dat_raw1 %>%
  rbind(filter(comp_dat_raw1, SPECIES_GROUP %in% c('NEARSHORE GROUNDFISH',
                                                   'OFFSHORE GROUNDFISH',
                                                   'NON-WHITING GROUNDFISH NON-IFQ',
                                                   'NON-WHITING GROUNDFISH IFQ',
                                                   'MIDWATER')) %>%
          mutate(SPECIES_GROUP = 'ALL NON-WHITING') %>%
          group_by(YEAR, LANDING_MONTH, LANDING_DATE, AGENCY_CODE, VESSEL_NUM, DEALER_NUM, SPECIES_GROUP) %>%
          summarize(ROUND_WEIGHT_MTONS = sum(ROUND_WEIGHT_MTONS),
                    EXVESSEL_REVENUE = sum(EXVESSEL_REVENUE)) %>%
          data.frame())

# Add in price metric and Remove outliers
comp_dat_outadj <- comp_dat_raw %>%
  mutate(price = EXVESSEL_REVENUE/(ROUND_WEIGHT_MTONS*2204.62)) %>%
  filter(price < 150) 

# Data formatting ####
# Adding date variables to use in data summaries
comp_dat_fmt <- comp_dat_outadj %>%
  mutate(WEEKOFYEAR = as.numeric(format(LANDING_DATE, "%U")),
         DAYOFYEAR = as.numeric(format(LANDING_DATE, "%j"))) 

comp_dat_sub <- comp_dat_fmt %>%
  select( -LANDING_DATE, -DAYOFYEAR) %>%
  reshape2::melt(c('VESSEL_NUM','DEALER_NUM','SPECIES_GROUP','YEAR','WEEKOFYEAR', 'LANDING_MONTH', 'AGENCY_CODE')) %>%
  rename(Metric = variable,
         Value = value) %>%
  # Deflator is being used here #
  merge(defl_adj) %>%
  mutate(Value = case_when(Metric %in% c('EXVESSEL_REVENUE', 'price') ~ Value/DEFL,
                           T ~ Value)) %>%
  select(-DEFL)

# Add agency_code = 'All' option for no state grouping
comp_dat_all <- comp_dat_sub %>%
  mutate(AGENCY_CODE = 'All') %>%
  rbind(comp_dat_sub) %>%
  # summarizing by month
  group_by(YEAR, VESSEL_NUM, DEALER_NUM, SPECIES_GROUP, LANDING_MONTH, AGENCY_CODE, Metric) %>%
  summarize(Value = sum(Value)) %>%
  mutate(CONF = 'NOT_TREATED') %>%
  data.frame()

saveRDS(comp_dat_all, "comp_dat_all.RDS")

# Add in weekly data 
comp_dat_all_wk <- comp_dat_sub %>%
  mutate(AGENCY_CODE = 'All') %>%
  rbind(comp_dat_sub) %>%
  # Summarizing by week
  group_by(YEAR, VESSEL_NUM, DEALER_NUM, SPECIES_GROUP, WEEKOFYEAR, AGENCY_CODE, Metric) %>%
  summarize(Value = sum(Value)) %>%
  mutate(CONF = 'NOT_TREATED',
         Interval = 'Weekly') %>%
  rename(LANDING_MONTH = WEEKOFYEAR) %>%
  data.frame() %>%
  rbind(comp_dat_all %>%
          mutate(Interval = 'Monthly'))

# run the confidentiality checks and suppress fields as necessary in the raw data
confidentiality <- confTreat(comp_dat_all_wk, c('YEAR','SPECIES_GROUP','LANDING_MONTH','AGENCY_CODE','Metric', 'Interval'),
           valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'), whichtables = 'both')

# save the treated data from confidentiality check
comp_dat_all_mtreated <- mutate(confidentiality$data, CONF = 'TREATED')

# dataframe with both the treated and untreated data
comp_dat_full <- rbind(comp_dat_all_wk, comp_dat_all_mtreated)

# Data analysis ####
# Calculating mean rev/mt by species group, agency_code, and month
comp_dat_dt <- data.table(comp_dat_full)
comp_dat_dt <- comp_dat_dt[!is.na(VESSEL_NUM)]
comp_dat_dt <- comp_dat_dt[, .(Value=sum(Value)), by=list(VESSEL_NUM, SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF, Interval)]
comp_dat_avg <- comp_dat_dt[, .(Mean = mean(Value), 
                                Median = median(Value), 
                                Variance = sd(Value), 
                                q25 = quantile(Value, prob =.25, type = 8, na.rm = T),
                                q75 = quantile(Value, prob =.75, type = 8, na.rm = T),
                                N = length(unique(VESSEL_NUM))), 
  by=.(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF, Interval)] %>%
  melt(c('SPECIES_GROUP','AGENCY_CODE', 'YEAR', 'LANDING_MONTH', 'Metric','Variance','q25','q75', 'CONF', 'N', 'Interval')) %>%
  rename(Statistic = variable,
         Value = value) %>%
  data.frame()

# Calculating the total rev/mt by species group, agency_code, and month  
comp_dat_tot_revlbs <- filter(comp_dat_full, Metric != 'price') %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF, Interval) %>%
  summarize(Value = sum(Value),
            N = length(unique(VESSEL_NUM)))

comp_dat_tot_price <- comp_dat_full %>%
  dcast(YEAR + VESSEL_NUM + DEALER_NUM + SPECIES_GROUP + LANDING_MONTH + 
          AGENCY_CODE + CONF + Interval ~ Metric, value.var = 'Value') %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, CONF, Interval) %>%
  summarize(EXVESSEL_REVENUE = sum(EXVESSEL_REVENUE),
            ROUND_WEIGHT_MTONS = sum(ROUND_WEIGHT_MTONS),
            Value = EXVESSEL_REVENUE/(ROUND_WEIGHT_MTONS*2204.62),
            N = length(unique(VESSEL_NUM))) %>%
  select(-EXVESSEL_REVENUE,-ROUND_WEIGHT_MTONS) %>%
  mutate(Metric = 'price')

  
comp_dat_tot <- rbind(comp_dat_tot_revlbs, comp_dat_tot_price) %>%
  mutate(Statistic = 'Total',
         q25 = NA_real_,
         q75 = NA_real_,
         Variance = NA_real_)%>%
  data.frame()

#Treat as confidential or not? Currently treating as confidential
# Number of vessels and dealers by species group, agency code, and month
comp_dat_n <- filter(comp_dat_full, !is.na(VESSEL_NUM) & !is.na(DEALER_NUM) & CONF == 'TREATED') %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, CONF, Interval) %>%
  summarize(`Number of vessels` = length(unique(VESSEL_NUM)),
            `Number of buyers` = length(unique(DEALER_NUM))) %>%
  melt(c('SPECIES_GROUP','AGENCY_CODE', 'YEAR', 'LANDING_MONTH', 'CONF','Interval')) %>%
  rename(Metric = variable,
         Value = value) %>%
  mutate(Statistic = 'Total',
         q25 = NA_real_,
         q75 = NA_real_,
         Variance = NA_real_,
         N = Value) %>%
  data.frame()

# Bind together data frames
comp_dat_final <- rbind(comp_dat_avg, comp_dat_tot, comp_dat_n) %>%
  rename(State = AGENCY_CODE,
         Species = SPECIES_GROUP,
         Year = YEAR) %>% 
  data.frame()

# Cut 35 are calculated using the untreated data ####
# We need to remove 2015/2016 disaster years from the calculation of 35% for crab
cut35_crab <- filter(comp_dat_final, !Year %in% c(2015, 2016, 2020) & CONF == 'NOT_TREATED' 
                     & grepl('CRAB', Species)) %>%
  group_by(Species, State, LANDING_MONTH, Metric, Statistic, Interval) %>%
  summarise(Value = median(Value)) %>%
  mutate(Year = 'Baseline') %>%
  data.frame()

cut35_sardine <- filter(comp_dat_final, !Year %in% 2015:2020 & CONF == 'NOT_TREATED' 
                        & Species == 'SARDINE') %>%
  group_by(Species, State, LANDING_MONTH, Metric, Statistic, Interval) %>%
  summarise(Value = median(Value)) %>%
  mutate(Year = 'Baseline') %>%
  data.frame()

cut35 <- subset(comp_dat_final, Year != 2020 & CONF == 'NOT_TREATED' 
                & !grepl('CRAB', Species) & Species != 'SARDINE') %>%
  group_by(Species, State, LANDING_MONTH, Metric, Statistic, Interval) %>%
  summarise(Value = median(Value)) %>%
  mutate(Year = 'Baseline') %>%
  data.frame()

cut35_dat <- rbind(cut35, cut35_crab, cut35_sardine) %>%
  mutate(CONF = 'NOT_TREATED',
         q25 = NA_real_,
         q75 = NA_real_,
         Variance = NA_real_,
         N = "")

# baseline comparison
cutoff_2020 <- subset(comp_dat_final, Year == 2020) %>%
  group_by(Interval) %>%
  summarize(LANDING_MONTH = max(LANDING_MONTH))

only_2020 <- filter(comp_dat_final, 
  # pull the untreated values
  CONF == 'NOT_TREATED' &
  Metric == 'EXVESSEL_REVENUE' & 
  Statistic == 'Total' &
  Year == '2020' &
  Interval == 'Weekly') %>%
  group_by(Species, State) %>%
  mutate(cumREV_2020 = cumsum(Value)) %>%
  right_join(cutoff_2020) %>%
  select(Species, State, cumREV_2020)  %>%
  subset(!is.na(Species))

baseline <- filter(comp_dat_final, 
  # take care of crab disaster
  !(Year %in% c(2015, 2016) & grepl('CRAB', Species)) &
  # take care of sardine disasters
  !(Year %in% c(2015, 2016, 2017, 2018, 2019) & Species == 'SARDINE')  &
  # pull the untreated values
  CONF == 'NOT_TREATED' &
  Metric == 'EXVESSEL_REVENUE' & 
  Statistic == 'Total' &
  Year != '2020' &
  Interval == 'Weekly' &
  !is.na(Metric)) %>%
  group_by(Species, State, Year) %>%
  mutate(cumREV = cumsum(Value)) %>%
  right_join(cutoff_2020) %>%
  group_by(Species, State) %>%
  summarise(cumREV_hist = median(cumREV))%>%
  subset(!is.na(Species))

baseline_2020 <- full_join(only_2020, baseline) %>%
  mutate(percchange = (cumREV_2020-cumREV_hist)/cumREV_hist*100) %>%
  mutate(percdiff = percdiff(cumREV_hist, cumREV_2020)) %>%
  select(-cumREV_2020, -cumREV_hist) %>%
  data.table()
  
    
# all data including the 35% cutoff data
comp_dat_final_cut <- rbind(comp_dat_final, cut35_dat) %>%
  mutate(Cumulative = 'N')


# comp_dat_final_cumul <- subset(comp_dat_final_cut, Statistic == 'Total' 
#                                & Metric %in% c('ROUND_WEIGHT_MTONS', 'EXVESSEL_REVENUE') 
#                                & CONF == 'TREATED') %>%
#   group_by(Species, State, Year, Metric, Statistic, Interval) %>%
#   mutate(Value = cumsum(Value)) %>%
#   mutate(Cumulative = 'Y') %>%
#   data.frame() %>%
#   rbind(comp_dat_final_cut) %>%
#   mutate(rm_conf = case_when(Cumulative == 'Y' ~ 0,
#                              Year == 'cut35' ~ 0,
#                              Metric %in% c('Number of vessels', 'Number of buyers') ~ 0,
#                              CONF == 'NOT_TREATED' ~ 1,
#                              T ~ 0)) %>%
#   filter(rm_conf != 1) %>%
#   select(-rm_conf,-CONF) %>%
#   ungroup()

conftable <- rename(confidentiality$flag,
  State = AGENCY_CODE,
  Species = SPECIES_GROUP,
  Year = YEAR) %>%
  mutate(Year = as.character(Year))

comp_dat_final_cumul <- subset(comp_dat_final_cut, Statistic == 'Total' 
                               & Metric %in% c('ROUND_WEIGHT_MTONS', 'EXVESSEL_REVENUE') 
                               & CONF == 'NOT_TREATED') %>%
  group_by(Species, State, Year, Metric, Statistic, Interval) %>%
  mutate(Value = cumsum(Value),
    Cumulative = 'Y') %>%
  # join on the confidentiality table and suppress as needed
  left_join(conftable) %>%
  mutate(Value = ifelse(final == 'ok' | is.na(final), Value, NA)) %>%
  mutate(CONF = 'TREATED') %>%
  select(-final) %>%
  data.frame() %>%
  rbind(comp_dat_final_cut) %>%
  mutate(rm_conf = case_when(Cumulative == 'Y' ~ 0,
                             Year == 'Baseline' ~ 0,
                             Metric %in% c('Number of vessels', 'Number of buyers') ~ 0,
                             CONF == 'NOT_TREATED' ~ 1,
                             T ~ 0)) %>%
  filter(rm_conf != 1) %>%
  select(-rm_conf,-CONF)


all_combos <- comp_dat_final_cumul %>%
  select(Year, State, LANDING_MONTH, Statistic, Metric, Cumulative, Interval) %>%
  expand(Year, State, LANDING_MONTH, Statistic, Metric, Cumulative, Interval) %>%
  merge((comp_dat_final_cumul %>%
           select(Species) %>%
           distinct()), all = T) %>%
  #group_by(Year, State, LANDING_MONTH, Statistic, Metric, Cumulative, Type, Interval, Species) %>%
  #distinct() %>%
  mutate(rm = case_when(Interval == 'Monthly' & (LANDING_MONTH > 12 | LANDING_MONTH < 1) ~ 1,
                        Interval == 'Monthly' & LANDING_MONTH > month(completeness_cutoff) 
                        & Year == 2020 ~ 1,
                        Interval == 'Weekly' & LANDING_MONTH > week(today) & Year == 2020 ~ 1,
                        T ~ 0)) %>%
  filter(rm != 1) %>%
  select(-rm) 
    

# Add in 0s for clarity between suppressed v. no data. Without this step combinations with 0 don't show up at all. 
comp_dat_final_cumul_0s <- merge(all_combos, comp_dat_final_cumul, all.x = T)

# add filter for fisheries
# proportion within state

sharewithinstate <- subset(comp_dat_all,
    Metric == 'EXVESSEL_REVENUE' & 
    YEAR %in% 2014:2019) %>%
  group_by(AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(Value = sum(Value, na.rm = T)) %>%
  ungroup() %>%
  group_by(AGENCY_CODE) %>%
  mutate(state_prop = (Value/sum(Value, na.rm = T))*100) %>%
  select(-Value)

sharewithinmonth <- subset(comp_dat_all,
    Metric == 'EXVESSEL_REVENUE' & 
    YEAR %in% 2014:2019) %>%
  group_by(AGENCY_CODE, SPECIES_GROUP, LANDING_MONTH) %>%
  summarize(Value = sum(Value, na.rm = T)) %>%
  ungroup() %>%
  group_by(AGENCY_CODE, SPECIES_GROUP) %>%
  mutate(month_prop = (Value/sum(Value, na.rm = T))*100) %>%
  mutate(select_month = factor(month.abb[LANDING_MONTH], levels = month.abb)) %>%
  select(-LANDING_MONTH, -Value)

addlfilters <- full_join(sharewithinstate, sharewithinmonth) %>%
  rename(Species = SPECIES_GROUP,
    State = AGENCY_CODE) %>%
  full_join(baseline_2020) %>%
  ungroup() %>%
  mutate(Species = convert_sp(Species),
    State = convert_state(State),
    month_prop = case_when(is.na(month_prop) ~ NA_character_,
                           month_prop <= 5 ~ '0-5%',
                           month_prop > 5 & month_prop <= 10 ~ '> 5%, <= 10%',
                           month_prop > 10 & month_prop <= 15 ~ '> 10%, <= 15%',
                           month_prop > 15 & month_prop <= 20 ~ '> 15%, <= 20%',
                           month_prop > 20 ~ '> 20%',
                           T ~ 'help'),
    percchange = case_when(is.na(percchange) ~ NA_character_,
                           percchange <= -35 ~ '-35% or less',
                           percchange > -35 & percchange <= 0 ~ '> -35%, <= 0%',
                           percchange > 0 & percchange <= 35 ~ '> 0%, <= 35%',
                           percchange > 35 ~ '> 35%',
                           T ~ 'help'),
    state_prop = case_when(is.na(state_prop) ~ NA_character_,
                           state_prop <= 5 ~ '0-5%',
                           state_prop > 5 & state_prop <= 10 ~ '> 5%, <= 10%',
                           state_prop > 10 ~ '> 10%',
                           T ~ 'help')
    ) %>%
  data.frame()

saveRDS(addlfilters, "addlfilters.RDS")

# explore props
# subset(sharewithinstate, AGENCY_CODE != 'F') %>%
# ggplot(aes(x = SPECIES_GROUP, y = state_prop)) +
#   facet_wrap(~AGENCY_CODE) +
#   geom_col()

# Final formatting ####
app_data <-  comp_dat_final_cumul_0s %>%
  mutate(Metric = case_when(Metric == 'EXVESSEL_REVENUE' ~ 'Exvessel revenue',
                            Metric == 'ROUND_WEIGHT_MTONS' ~ 'Landed weight',
                            Metric == 'price' ~ 'Price (per lb)',
                            T ~ as.character(Metric)),
         Species = convert_sp(Species),
         State = convert_state(State),
         Type = ifelse(Year %in% 2014:2019, '2014-2019',
                       Year),
         # When we do the all combos merge if data is missing it shows up as NA.
         N = case_when(is.na(N) & Type != 'Baseline' & Cumulative != 'Y' ~ 0,
                           T ~ as.numeric(N)),
         Value = case_when(N == 0 ~ 0,
                       T ~ Value)) %>%
  group_by(Metric, Statistic, Cumulative, Interval) %>%
  mutate(unit = case_when(max(Value, na.rm = T) < 1e3 ~ '',
                          max(Value, na.rm = T) < 1e6 ~ 'thousands',
                          max(Value, na.rm = T) < 1e9 ~ 'millions',
                          max(Value, na.rm = T) < 1e12 ~ 'billions'),
         Variance = case_when(Year != 2020 ~ NA_real_,
                              T ~ Variance),
         q25 = case_when(Year != 2020 ~ NA_real_, 
                         T ~ q25),
         q75 = case_when(Year != 2020 ~ NA_real_,
                         T ~ q75)
    )  %>%
  ungroup() %>%
  mutate(
    Year = as.factor(Year),
    Value = case_when(
      unit == '' ~ Value,
      unit == 'thousands' ~ Value/1e3,
      unit == 'millions' ~ Value/1e6,
      unit == 'billions' ~ Value/1e9,
      T ~ -999),
    Variance = case_when(
      unit == '' ~ Variance,
      unit == 'thousands' ~ Variance/1e3,
      unit == 'millions' ~ Variance/1e6,
      unit == 'billions' ~ Variance/1e9,
      T ~ -999),
    q25 = case_when(
      unit == '' ~ q25,
      unit == 'thousands' ~ q25/1e3,
      unit == 'millions' ~  q25/1e6,
      unit == 'billions' ~  q25/1e9,
      T ~ -999),
    q75 = case_when(
      unit == '' ~ q75,
      unit == 'thousands' ~ q75/1e3,
      unit == 'millions' ~  q75/1e6,
      unit == 'billions' ~  q75/1e9,
      T ~ -999),
    ylab = case_when(Metric %in% c('Exvessel revenue', 'Price (per lb)') ~
                       paste0(State, ": ", Species, "\n(", unit, " 2019$)"),
                     Metric == 'Landed weight' ~
                       paste0(State, ": ", Species, "\n(", unit, " mt)"),
                     Metric == 'Number of vessels' ~
                       paste0(State, ": ", Species, "\n(", unit, ")"),
                     T ~ paste(State, ": ", Species)),
    upper = case_when(Statistic == 'Mean' ~ Value + Variance,
                      Statistic == 'Median' ~ q75,
                      Statistic == 'Total' ~ Value),
    lower = case_when(Statistic == 'Mean' ~ Value - Variance,
                      Statistic == 'Median' ~ q25,
                      Statistic == 'Total' ~ Value),
    Type = ifelse(Type == 'Baseline', '2014-2019 Median', Type),
    Type = factor(Type, levels = c('2014-2019', '2014-2019 Median', '2020')),
    # Formatting the date so it can be plotted appropriately
    Date = case_when(Interval == 'Weekly' & LANDING_MONTH < 2 ~ as.Date(paste0(Year,'-01-01')),
                     Interval == 'Weekly' & LANDING_MONTH > 1 ~ as.Date(lubridate::parse_date_time(
                       paste(Year, LANDING_MONTH, 'Sun', sep="/"),'Y/W/a')),
                     Interval == 'Monthly' ~ ymd(paste0(Year, '-', LANDING_MONTH, '-01'))),
    LANDING_MONTH = case_when(Interval == 'Weekly' & LANDING_MONTH < 2 ~ as.Date('2001-01-01'),
                              Interval == 'Weekly' & LANDING_MONTH > 1 ~ as.Date(lubridate::parse_date_time(
                                paste(2001, LANDING_MONTH, 'Sun', sep="/"),'Y/W/a')),
                              Interval == 'Monthly' ~ ymd(paste0('2001', LANDING_MONTH, '-01'))),
    # apply completeness/uncertainty logic. best understanding is that WA is 4 week lag, apply different logic for WA
    complete = case_when(State == 'Washington' & Statistic == 'Total' & Interval == 'Weekly' 
                          & Date >= wk4_completeness ~ "uncertain",
                         State == 'Washington' & Statistic == 'Total' & Interval == 'Monthly' & Year == 2020 &
                           month(Date) > wk4_month ~ "uncertain",
                         Interval == 'Weekly' & Date >= completeness_cutoff ~ "uncertain",
                         Interval == 'Monthly' & Year == 2020 & month(Date) > month_cutoff ~ "uncertain",
                         T ~ "complete")) %>%
  data.frame() 

saveRDS(app_data, "comp_dat_covidapp.RDS")
#write.fst(app_data, "comp_dat_covidapp.fst")

# numbers of years where 2020 is lower

# only2020 <- subset(comp_dat_final, Year == 2020) %>%
#   subset(Metric == 'Exvessel revenue' & Statistic == 'Total') %>%
#   select(Species, State, LANDING_MONTH, Value) %>%
#   rename(yr_2020 = Value)
# 
# comp_table <- subset(comp_dat_final, Year != 2020) %>%
#   subset(Metric == 'Exvessel revenue' & Statistic == 'Total' & LANDING_MONTH %in% c('Jan', 'Feb', 'Mar')) %>%
#   select(Species, State, LANDING_MONTH, Year, Value) %>%
#   full_join(only2020) %>%
#   mutate(Value = ifelse(is.na(Value), 0, Value)) %>%
#   mutate(flag = ifelse(yr_2020 < .65*Value, '35 or more', 'less than 35')) %>%
#   ungroup() %>%
#   group_by(Species, State, LANDING_MONTH, flag) %>%
#   summarise(N = length(flag)) %>%
#   subset(flag == '35 or more' & State != 'Coastwide') %>%
#   subset(LANDING_MONTH == 'Mar') %>%
#   dcast(Species ~ State, value.var = 'N')
