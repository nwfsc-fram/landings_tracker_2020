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

#' Confidentiality clean up of data
#'
#' @param x data.frame of data
#' @param vector of variables the grouping variables for checking confidentiality, e.g.: c("YEAR", "Category")
#' @param valvar the column of values that will be reported/need to be checked
#' @param confunit vector of units that need to be checked, most common is: c('VESSEL_ID', 'BUYER_ID')
#' @param zeroNAtrt not currently implemented
#' @param drop  not currently implemented
#' @param whichtables output type, options are datatable, flagtable or both
#' @param origvalname for the output, name of the original data column (unsuppressed data) (optional)
#' @param finalvalname for the output, name of the final column (optional)
#' @param use3 logical whether to include the rule of 3 in confidentiality check/suppression
#' @param use90 logical whether to inclue the 90/10 rule in confidentiality check/suppression
#' @param aggregate not currently implemented
#' @param inclorig logical whether to include the unsuppressed column in output
#'
#' @return
#' @export
#'
#' @examples
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
  aggregate = FALSE,
  inclorig = FALSE) {
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
  if( use3 == TRUE & use90 == TRUE) {

    flagtable_red <- flagtable_int

  } else if(use3 == TRUE & use90 == FALSE) {

    flagtable_red <- flagtable_int[variable == 'rule3']

  } else if(use3 == FALSE & use90 == TRUE) {

    flagtable_red <- flagtable_int[variable == 'rule90']

  } else {

    stop(
      "Both use3 and use90 are set to FALSE, no confidentiality checking will be performed, please set at least one to TRUE"
    )

  }
    flagtable <- flagtable_red[, .(final = case_when(any(value == 'suppress') ~ 'suppress', T ~ 'ok' )), by = mget(variables)]

  # implement the confidential data suppression of data
  final_data <- dat[flagtable, on = variables][,newvals := ifelse(final == 'suppress', NA, aggvals)][,final:=NULL]

  if(inclorig == F) final_data[,aggvals:=NULL]

  # rename columns accordig to function inputs
  setnames(final_data, 'aggvals', paste0(valvar, "orig"), skip_absent = TRUE)
  setnames(final_data, 'newvals', valvar)

  if(whichtables == 'datatable') {

    return(data.frame(final_data))

  } else if (whichtables == 'flagtable') {

    return(data.frame(flagtable))

  } else {

    bothtables <- list(data.frame(final_data), data.frame(flagtable))

    names(bothtables) <- c('data', 'flags')

    return(bothtables)

  }


}
# #
# both <- confTreat(
#   xchk,
#   variables = c('YEAR', 'shortdescr'),
#   valvar = 'D_NUMBER_RESPONSE',
#   confunit = c('VESSEL_ID', 'BUYER_ID')
# )
#
# table(is.na(both$D_NUMBER_RESPONSE))

# no3 <- confTreat(
#   xchk,
#   variables = c('YEAR', 'shortdescr'),
#   valvar = 'D_NUMBER_RESPONSE',
#   confunit = c('VESSEL_ID', 'BUYER_ID'),
#   use90 = FALSE
# )
#
# table(is.na(no3$D_NUMBER_RESPONSE))


#dtdat <- data.table(xchk)
