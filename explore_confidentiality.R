# This script pulls data and formats the dataframe for use in the app #
library(dplyr)
library(reshape2)
library(lubridate)

comp_dat_raw <- readRDS('comp_dat_raw.RDS') %>%
  rename(YEAR = LANDING_YEAR)