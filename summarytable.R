comp_dat_raw <- readRDS('comp_dat_raw.RDS')

weekly_by_state <- mutate(comp_dat_raw, 
  week = week(LANDING_DATE)) %>%
  group_by(LANDING_YEAR, week, AGENCY_CODE, SPECIES_GROUP) %>%
  summarise(rev = sum(EXVESSEL_REVENUE), mts = sum(ROUND_WEIGHT_MTONS))

weekly <- mutate(comp_dat_raw, 
  week = week(LANDING_DATE)) %>%
  group_by(LANDING_YEAR, week, SPECIES_GROUP) %>%
  summarise(rev = sum(EXVESSEL_REVENUE), mts = sum(ROUND_WEIGHT_MTONS))

sofar <- mutate(comp_dat_raw,
  period = ifelse(LANDING_MONTH < 4, 'now', 'later')) %>%
  group_by(period, SPECIES_GROUP) %>%
  summarise(rev = sum(EXVESSEL_REVENUE)) %>%
  mutate(prop = rev/sum(rev)) %>%
  subset(period == 'now') %>%
  arrange(desc(prop)) %>%
  subset(prop > .01) %>%
  data.frame()

topsp <- sofar[,'SPECIES_GROUP']
subset(weekly, SPECIES_GROUP %in% topsp) %>%
  mutate(SPECIES_GROUP = factor(SPECIES_GROUP, levels = c(topsp))) %>%
ggplot(aes(x = week, y = rev, group = LANDING_YEAR)) +
  geom_line() +
  facet_wrap(~SPECIES_GROUP, scales = 'free_y')
