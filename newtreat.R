# library(EDCReport)
# data(dummyEDCdata)
# crosscheck <- dummyEDCdata[,c('VESSEL_ID', 'YEAR', 'shortdescr', 'D_NUMBER_RESPONSE')] %>%
#   arrange(YEAR, shortdescr) %>%
#   mutate(VESSEL_ID = ifelse(YEAR == 2009 & shortdescr == 'Cost B' & VESSEL_ID != '119258', '668174', VESSEL_ID)) %>%
#   mutate(VESSEL_ID = ifelse(YEAR == 2009 & shortdescr == 'Cost A' & VESSEL_ID != '119258', '668174', VESSEL_ID))
#
# crosscheck$BUYER_ID = c(rep(c('FR1', 'FR2', 'FR3'), each = 3), rep(c('FR1', 'FR2'), each = 4))
#
# xchk <- rbind(crosscheck, crosscheck[1:10,])
#
#
# dummyEDCdata

require(data.table)

confTreat <- function(x,
  variables,
  valvar,
  confunit,
  zeroNAtrt = 'asis',
  drop = TRUE,
  whichtables = 'datatable',
  origvalname = paste0(valvar, "orig"),
  finalvalname = valvar,
  use3 = TRUE,
  use90 = TRUE,
  aggregate = FALSE) {
  # warnings
  try(if (!zeroNAtrt %in% c('zeroasNA', 'NAaszero', 'asis'))
    stop(
      "zeroNAtreatment must be defined as either zeroasNA or NAaszero or asis, if nothing is specified then it defaults to asis"
    ))
  
  try(if (any("flag" %in% variables))
    stop("variables cannot contain the field 'flag'"))
  
  try(if (origvalname == finalvalname)
    stop("origvalname cannot have the same value as finalvalname"))
  
  try(if (origvalname == 'finalnum')
    stop("origvalname cannot have the value 'finalnum'"))
  
  try(if (any(variables %in% valvar))
    stop("valvar cannot be any of the components of the variables"))
  
  try(if (!(valvar %in% names(x)))
    stop(paste(valvar, "is not present in the data provided")))
  
  try(if (!whichtables %in% c('flagtable', 'datatable', 'both'))
    stop(
      "whichtables must be defined as flagtable, datatable, both, if nothing is specified then it defaults to datatable"
    ))
  
  # convert data input into data.table
  dat <- data.table(x)
  
  # rename the value column so that it's easier to reference in the code
  
  setnames(dat, valvar, "aggvals")
  
  # aggregate the data or confirm that the data are already aggregated
  
  flagtable_int <- NULL
  
  # look through each of the variables in confunit
  for (i in 1:length(confunit)) {
    # sum data over the grouping variables being evaluated
    aggdat <- dat[, .(aggvals = sum(aggvals)), by = c(variables, confunit[i])]
    # calculate proportion of total for rule of 90/10
    aggdat[, prop := aggvals / sum(aggvals), by = c(variables)]
    # calculate Ns for rule of 3
    aggdat[, N    := length(aggvals), by = c(variables)]
    # flag any combos that are flagged by either rule (within one confunit)
    aggdat <-
      aggdat[, .(
        rule90 = case_when(any(prop > .9) ~ 'suppress', T ~ 'ok'),
        rule3 = case_when(any(N < 3) ~ 'suppress', T ~ 'ok')
      ),
        by = variables]
  #browser()      
    # reshape the outputs and combine with previous runs of loop
    aggdat <-
      melt(aggdat[, mget(c(variables, 'rule3', 'rule90'))], variables)
    aggdat$source = confunit[i]
    
    flagtable_int <- rbind(aggdat, flagtable_int)

  }
  
  # combine the flags for all levels of confunit and reevaluate 
  flagtable <- flagtable_int[, .(final = case_when(
    use3 == TRUE & use90 == TRUE & any(value == 'suppress') ~ 'suppress',
    T ~ 'ok'
  )), by = mget(variables)]
  
  # implement the confidential data suppression of data
  final <- dat[flagtable, on = variables][,newvals := ifelse(final == 'suppress', NA, aggvals)][,final:=NULL]
  
  # rename columns accordig to function inputs
  setnames(final, 'aggvals', paste0(valvar, "orig"))
  setnames(final, 'newvals', valvar)
  
  # final data frame
  return(data.frame(final))
  
  
}
# 
# confTreat(
#   xchk,
#   variables = c('YEAR', 'shortdescr'),
#   valvar = 'D_NUMBER_RESPONSE',
#   confunit = c('VESSEL_ID', 'BUYER_ID')
# )

#dtdat <- data.table(xchk)
