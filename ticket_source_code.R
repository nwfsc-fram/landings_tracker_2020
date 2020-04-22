# Re-ran data_pull and included "ticket_source_code" variable
# Also noticed that the data pull excludes etix. do we want that?

#Conclusion, there is very little "E" ticket_source_code. The biggest place this occurs is
# California Dungness Crab, but even still, the other sources outweigh by a lot.

# Analysis of E v R in data
comp_dat_raw <- readRDS('comp_dat_raw.RDS') %>%
  rename(YEAR = LANDING_YEAR) %>%
  filter(YEAR == 2020)

e_vs_r <- comp_dat_raw %>%
  group_by(TICKET_SOURCE_CODE, AGENCY_CODE, LANDING_MONTH, SPECIES_GROUP) %>%
  summarize(rev = sum(EXVESSEL_REVENUE),
            n = length(unique(VESSEL_NUM)))

# California tickets ####
ggplot(filter(e_vs_r,AGENCY_CODE == 'C'), aes(x = TICKET_SOURCE_CODE, y = rev)) +
  geom_col() +
  facet_wrap(.~SPECIES_GROUP)

# Oregon tickets ####
ggplot(filter(e_vs_r,AGENCY_CODE == 'O'), aes(x = TICKET_SOURCE_CODE, y = rev)) +
  geom_col() +
  facet_wrap(.~SPECIES_GROUP)

# Washington tickets ####
ggplot(filter(e_vs_r,AGENCY_CODE == 'W'), aes(x = TICKET_SOURCE_CODE, y = rev)) +
  geom_col() +
  facet_wrap(.~SPECIES_GROUP)
