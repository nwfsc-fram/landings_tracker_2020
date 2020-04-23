

thefiles <- list.files()[grepl('comp_dat_raw2020', list.files())]

alldat <- NULL

for(f in 1:length(thefiles)) {
  
  file_name = thefiles[f]
  print(file_name)
  datRaw <- readRDS(file_name) %>%
    subset(LANDING_YEAR == 2020) %>%
    group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DATE, AGENCY_CODE) %>%
    summarise(N_vss = length(unique(VESSEL_NUM)),
      N_proc = length(unique(DEALER_NUM)),
      REV = sum(EXVESSEL_REVENUE),
      MTS = sum(ROUND_WEIGHT_MTONS)) %>%
    mutate(source = file_name)

  alldat <- rbind(alldat, datRaw)
  #print(table(alldat$source))
  
}

saveRDS(alldat, "completenessdata.rds")

addl <- alldat %>%
  ungroup() %>%
  group_by(source, AGENCY_CODE) %>%
  mutate(cumREV = cumsum(REV),
    cumMTS = cumsum(MTS)) %>%
  mutate(pull_date = ymd(gsub('.RDS', '', gsub('comp_dat_raw', '', source))))

ggplot(subset(addl, LANDING_DATE > ymd('2020-03-14')), aes(x = LANDING_DATE, y = cumMTS)) + 
  geom_line(aes(color = source)) + 
  facet_grid(AGENCY_CODE ~ ., scales = 'free_y')

    

wadat <- NULL

for(f in 1:length(thefiles)) {
  
  file_name = thefiles[f]
  print(file_name)
  datRaw <- readRDS(file_name) %>%
    subset(LANDING_YEAR == 2020 & AGENCY_CODE == 'W') %>%
    group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DATE, SPECIES_GROUP) %>%
    summarise(N_vss = length(unique(VESSEL_NUM)),
      N_proc = length(unique(DEALER_NUM)),
      REV = sum(EXVESSEL_REVENUE),
      MTS = sum(ROUND_WEIGHT_MTONS)) %>%
    mutate(source = file_name)

  wadat <- rbind(wadat, datRaw)
  #print(table(wadat$source))
  
}

addlwa <- wadat %>%
  ungroup() %>%
  group_by(source, SPECIES_GROUP) %>%
  mutate(cumREV = cumsum(REV),
    cumMTS = cumsum(MTS)) %>%
  mutate(pull_date = ymd(gsub('.RDS', '', gsub('comp_dat_raw', '', source))))

ggplot(subset(addlwa, LANDING_DATE > ymd('2020-03-14') & SPECIES_GROUP != 'SHELLFISH'), aes(x = LANDING_DATE, y = cumMTS)) + 
  geom_line(aes(color = source)) + 
  facet_wrap(~SPECIES_GROUP, scales = 'free_y')

