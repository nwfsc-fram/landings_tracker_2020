# This script pulls data and formats the dataframe for use in the app #
library(dplyr)
library(reshape2)
library(lubridate)
library(EDCReport)

# Insert database connection information here #
source("C:/Program Files/R/connectioninfoROracle.r")

# Deflator #####
# Currently coded to treat 2020 as 2019$
defl <- structure(list(YEAR = c(2014, 2015, 2016, 2017, 2018, 2019), 
    DEFL = c(0.92239, 0.93203, 0.94167, 0.95938, 0.98276, 1)), row.names = 30:35, class = "data.frame")
defl2020 <- filter(defl, YEAR == 2019) %>%
  mutate(YEAR = 2020)
defl_adj <- rbind(defl, defl2020)

# Load data from data_pull.R ####
comp_dat_raw <- readRDS('comp_dat_raw.RDS') %>%
  rename(YEAR = LANDING_YEAR)

# Remove outliers
comp_dat_outadj <- comp_dat_raw %>%
  mutate(price = EXVESSEL_REVENUE/(ROUND_WEIGHT_MTONS*2204.62)) %>%
  filter(price < 150) %>%
  select(-price)

# Data formatting ####
# Adding date variables to use in data summaries
comp_dat_fmt <- comp_dat_outadj %>%
  mutate(WEEKOFYEAR = as.numeric(format(LANDING_DATE, "%U")),
         DAYOFYEAR = as.numeric(format(LANDING_DATE, "%j")))

comp_dat_sub <- filter(comp_dat_fmt#, DAYOFYEAR <= max_2020
  ) %>%
  select( -LANDING_DATE, -DAYOFYEAR) %>%
  melt(c('VESSEL_NUM','DEALER_NUM','SPECIES_GROUP','YEAR','WEEKOFYEAR', 'LANDING_MONTH', 'AGENCY_CODE')) %>%
  rename(Metric = variable,
         Value = value) %>%
  # Deflator is being used here #
  merge(defl_adj) %>%
  mutate(Value = case_when(Metric == 'EXVESSEL_REVENUE' ~ Value/DEFL,
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

comp_dat_all_mtreated <- comp_dat_all %>%
  PreTreat(c('YEAR','SPECIES_GROUP','LANDING_MONTH','AGENCY_CODE','Metric'),
           valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'), zeroNAtreatment = 'asis') %>%
  mutate(CONF = 'TREATED') %>%
  select(-Valueorig)

comp_dat_full <- rbind(comp_dat_all, comp_dat_all_mtreated)
# Data analysis ####
# Calculating mean rev/mt by species group, agency_code, and month
comp_dat_avg <- filter(comp_dat_full, !is.na(VESSEL_NUM)) %>% # we need to remove NAs from averages
  group_by(VESSEL_NUM, SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF) %>%
  # Summarize with dealer_num removed #
  summarize(Value = sum(Value)) %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF) %>%
  summarize(Mean = mean(Value),
            Median = median(Value),
            Variance = sd(Value),
            q25 = quantile(Value, prob =.25, type = 8, na.rm = T),
            q75 = quantile(Value, prob =.75, type = 8, na.rm = T)) %>%
  melt(c('SPECIES_GROUP','AGENCY_CODE', 'YEAR', 'LANDING_MONTH', 'Metric','Variance','q25','q75', 'CONF')) %>%
  rename(Statistic = variable,
         Value = value)
# Calculating the total rev/mt by species group, agency_code, and month  
comp_dat_tot <- comp_dat_full %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric, CONF) %>%
  summarize(Value = sum(Value)) %>%
  mutate(Statistic = 'Total',
         q25 = NA_real_,
         q75 = NA_real_,
         Variance = NA_real_) %>%
  data.frame()

# Treating this information as non-confidential
# Number of vessels and dealers by species group, agency code, and month
comp_dat_n <- filter(comp_dat_all, !is.na(VESSEL_NUM) & !is.na(DEALER_NUM)) %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH) %>%
  summarize(`Number of vessels` = length(unique(VESSEL_NUM)),
            `Number of buyers` = length(unique(DEALER_NUM))) %>%
  melt(c('SPECIES_GROUP','AGENCY_CODE', 'YEAR', 'LANDING_MONTH')) %>%
  rename(Metric = variable,
         Value = value) %>%
  mutate(Statistic = 'Total',
         q25 = NA_real_,
         q75 = NA_real_,
         Variance = NA_real_,
         CONF = 'NOT_TREATED') %>%
  data.frame()

comp_dat_final <- rbind(comp_dat_avg, comp_dat_tot, comp_dat_n) %>%
  mutate(Metric = case_when(Metric == 'EXVESSEL_REVENUE' ~ 'Exvessel revenue',
                            Metric == 'ROUND_WEIGHT_MTONS' ~ 'Landed weight',
                            T ~ as.character(Metric)),
         SPECIES_GROUP = case_when(SPECIES_GROUP == 'COASTAL PELAGIC' ~ 'Coastal pelagics',
                                   SPECIES_GROUP == 'CRAB' ~ 'Crab',
                                   SPECIES_GROUP == 'NON-WHITING GROUNDFISH' ~ 'Non-whiting groundfish',
                                   SPECIES_GROUP == 'TUNA' ~ 'Tuna',
                                   SPECIES_GROUP == 'OTHER' ~ 'Other species',
                                   SPECIES_GROUP == 'SALMON' ~ 'Salmon',
                                   SPECIES_GROUP == 'SHELLFISH' ~ 'Shellfish',
                                   SPECIES_GROUP == 'SHRIMP' ~ 'Shrimp',
                                   SPECIES_GROUP == 'WHITING' ~ 'Whiting',
           T ~ 'help'),
         AGENCY_CODE = case_when(AGENCY_CODE == 'O' ~ 'Oregon',
                                 AGENCY_CODE == 'W' ~ 'Washington',
                                 AGENCY_CODE == 'C' ~ 'California',
                                 AGENCY_CODE %in% c('All') ~ 'All states',
                                 AGENCY_CODE %in% c('F') ~ 'At-sea',
           T ~ 'help')) %>%
  group_by(Metric, Statistic) %>%
  mutate(unit = case_when(max(Value, na.rm = T) < 1e3 ~ '',
                          max(Value, na.rm = T) < 1e6 ~ 'thousands',
                          max(Value, na.rm = T) < 1e9 ~ 'millions',
                          max(Value, na.rm = T) < 1e12 ~ 'billions'),
         Variance = case_when(YEAR != 2020 ~ NA_real_,
                              T ~ Variance),
         q25 = case_when(YEAR != 2020 ~ NA_real_, 
                         T ~ q25),
         q75 = case_when(YEAR != 2020 ~ NA_real_,
                         T ~ q75),
         YEAR = as.factor(YEAR),
         ylab = case_when(Metric == 'Exvessel revenue' ~
                            paste0(AGENCY_CODE, ": ", SPECIES_GROUP, "\n(", unit, " 2019$)"),
                          Metric == 'Landed weight' ~
                            paste0(AGENCY_CODE, ": ", SPECIES_GROUP, "\n(", unit, " mt)"),
                          Metric == 'Number of vessels' ~
                            paste0(AGENCY_CODE, ": ", SPECIES_GROUP, "\n(", unit, ")"),
                          T ~ paste(AGENCY_CODE, ": ", SPECIES_GROUP)),
         LANDING_MONTH = month(LANDING_MONTH, label = TRUE)
    ) %>%
  rename(State = AGENCY_CODE,
         Species = SPECIES_GROUP,
         Year = YEAR)

# Cut 35 are calculated using the untreated data ####
cut35 <- subset(comp_dat_final, Year != 2020 & CONF == 'NOT_TREATED') %>%
  group_by(Species, State, LANDING_MONTH, Metric, Statistic, unit, ylab) %>%
  summarise(Value = median(Value) * .65) %>%
  mutate(Year = 'cut35')

comp_dat_final_cut <- rbind(comp_dat_final, cut35) %>%
  mutate(Type = ifelse(Year %in% 2014:2019, '2014-2019',
    Year),
    Cumulative = 'N')

comp_dat_final_cumul <- subset(comp_dat_final_cut, Statistic == 'Total' & Metric %in% c('Landed weight', 'Exvessel revenue') & CONF == 'NOT_TREATED') %>%
  group_by(Species, State, Year, Metric, Statistic, unit, ylab) %>%
  mutate(Value = cumsum(Value)) %>%
  mutate(Cumulative = 'Y') %>%
  rbind(comp_dat_final_cut) %>%
  mutate(rm_conf = case_when(Cumulative == 'Y' ~ 0,
                             Year == 'cut35' ~ 0,
                             Metric %in% c('Number of vessels', 'Number of buyers') ~ 0,
                             CONF == 'NOT_TREATED' ~ 1,
                             T ~ 0)) %>%
  filter(rm_conf != 1) %>%
  select(-rm_conf,-CONF)

comp_dat_final_cumul %>%
  data.frame() %>%
  mutate(
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
      upper = case_when(Statistic == 'Mean' ~ Value + Variance,
                        Statistic == 'Median' ~ q75,
                        Statistic == 'Total' ~ Value),
      lower = case_when(Statistic == 'Mean' ~ Value - Variance,
                        Statistic == 'Median' ~ q25,
                        Statistic == 'Total' ~ Value),
    Type = ifelse(Type == 'cut35', '35% threshold', Type)) %>%
  data.frame() %>%
saveRDS( "comp_dat_covidapp.RDS")

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
