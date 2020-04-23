
# Reformat species
convert_sp <- function(Species) {
  dat <-
    case_when(
      Species == 'OTHER COASTAL PELAGIC' ~ 'Other coastal pelagic',
      Species == 'ANCHOVY' ~ 'Anchovy',
      Species == 'SARDINE' ~ 'Sardine',
      Species == 'DUNGENESS CRAB' ~ 'Dungeness crab',
      Species == 'OTHER CRAB' ~ 'Other crab',
      Species == 'NON-WHITING GROUNDFISH NON-IFQ' ~ 'Non-whiting groundfish (non-IFQ)',
      Species == 'NON-WHITING GROUNDFISH IFQ' ~ 'Non-whiting groundfish (IFQ)',
      Species == 'TUNA' ~ 'Tuna',
      Species == 'OTHER' ~ 'Other species',
      Species == 'MARKET SQUID' ~ 'Market squid',
      Species == 'SALMON' ~ 'Salmon',
      Species == 'SHELLFISH' ~ 'Shellfish (excl. aquaculture)',
      Species == 'SHRIMP' ~ 'Shrimp',
      Species == 'WHITING' ~ 'Whiting',
      T ~ 'help'
    )
  
}

# Reformat states
convert_state <- function(State) {
  dat <- case_when(
    State == 'O' ~ 'Oregon',
    State == 'W' ~ 'Washington',
    State == 'C' ~ 'California',
    State %in% c('All') ~ 'All states',
    State %in% c('F') ~ 'At-sea',
    T ~ 'help'
  )
  
  return(dat)
  
}
