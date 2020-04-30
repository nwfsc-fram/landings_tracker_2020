
# Reformat species
convert_sp <- function(Species) {
  dat <-
    case_when(
      Species == 'OTHER COASTAL PELAGIC' ~ 'Other coastal pelagic',
      Species == 'ANCHOVY' ~ 'Anchovy',
      Species == 'SARDINE' ~ 'Sardine',
      Species == 'DUNGENESS CRAB' ~ 'Dungeness crab',
      Species == 'OTHER CRAB' ~ 'Other crab',
      Species == 'NEARSHORE GROUNDFISH' ~ 'Non-whiting groundfish (Fixed gear; nearshore)',
      Species == 'OFFSHORE GROUNDFISH' ~ 'Non-whiting groundfish (Fixed gear; non-nearshore)',
      Species == 'NON-WHITING GROUNDFISH NON-IFQ' ~ 'Non-whiting groundfish (Fixed gear; other)',
      Species == 'NON-WHITING GROUNDFISH IFQ' ~ 'Non-whiting groundfish (IFQ; bottom trawl)',
      Species == 'MIDWATER' ~ 'Non-whiting groundfish (IFQ; midwater trawl)',
      Species == 'TUNA' ~ 'Tuna',
      Species == 'OTHER' ~ 'Other species',
      Species == 'MARKET SQUID' ~ 'Market squid',
      Species == 'SALMON' ~ 'Salmon',
      Species == 'SHELLFISH' ~ 'Shellfish (excl. aquaculture)',
      Species == 'SHRIMP' ~ 'Shrimp',
      Species == 'WHITING' ~ 'Whiting',
      Species == 'PUGET SOUND FISHERIES' ~ 'Puget Sound fisheries',
      Species == 'ALL NON-WHITING' ~ 'All non-whiting groundfish',
      T ~ 'help'
    )
  
}

# Reformat states
convert_state <- function(State) {
  dat <- case_when(
    State == 'O' ~ 'Oregon',
    State == 'W' ~ 'Washington',
    State == 'C' ~ 'California',
    State %in% c('All') ~ 'All',
    State %in% c('F') ~ 'At-sea',
    T ~ 'help'
  )
  
  return(dat)
  
}
