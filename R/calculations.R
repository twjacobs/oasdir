

#' Indexed earnings are FICA earnings that have been adjusted using
#' a ratio of the National Averge Wage Index (AWI) for an individuals
#' indexing year over the Average Wage Index for the year the earnings
#' were realized. The indexing year is the year two years prior to
#' the Normal Retirement Age (NRA) year. In other words, it is the
#' year a person turns 60. This function returns the Average Indexed
#' Monthly Earnings (AIME) for the highest 35 years of indexed
#' earnings after the year a person turns 21. Note the AIME calculation
#' always uses 35 years of earnings even if it means some years of
#' $0 earnings must be included. If the endYear is in the future,
#' estimates of FICA earnings and AWI are used and appropriate
#' warnings are issued.
#'
#' @param ssStmt the Social Security Statement as retrieved through read_xml_ssStatement()
#' @param endYear the last year to include in the calculation
#' @return The Average Indexed Monthly Earnings set
#' @examples
#' getAimeSet(ssStmt = JQPublicStatement, endYear = 2021)
#'
#' @export
getAimeSet <- function(ssStmt = NULL, endYear = NULL){
  birthDate <- as.Date(ssStmt$userInformation$dateOfBirth)
  retireElgYear <- lubridate::year(birthDate + years(62))

  indexedEarnings <- getFicaEarnings(ssStmt, endYear = endYear)

  # Remove any earnings before the age of 22
  indexedEarnings <- indexedEarnings[indexedEarnings$startYear >=
                                 lubridate::year(ssStmt$userInformation$dateOfBirth) + 22,]

  ### Now calculate the Average Indexed Monthly Earnings
  # get base awi amount
  baseAwi <- getAwiValues(years =
                            lubridate::year(as.Date(ssStmt$userInformation$dateOfBirth) +
                                              lubridate::years(60)))
  # calculate factors
  factors <- getAwiSet(endYear = retireElgYear)
  factors <- factors[factors$Year %in% c(min(indexedEarnings$startYear):retireElgYear),]
  factors$factor <- baseAwi / factors$AWI
  factors$factor[which(factors$Year > (retireElgYear - 2))] <- 1.0

  # add the multiplying factors
  indexedEarnings$factors <- factors[match(indexedEarnings$startYear, factors$Year),]$factor
  indexedEarnings <- indexedEarnings[!is.na(indexedEarnings$factors),]
  indexedEarnings$adjEarnings <- indexedEarnings$ficaEarnings * indexedEarnings$factors

  # get highest (up to) 35 years of taxed social security wages
  indexedEarnings <- indexedEarnings[order(indexedEarnings$adjEarnings, decreasing = TRUE),]
  if(nrow(indexedEarnings > 35))
    indexedEarnings <- indexedEarnings[1:35,]

  indexedEarnings
}





#' Indexed earnings are FICA earnings that have been adjusted using
#' a ratio of the National Averge Wage Index (AWI) for an individuals
#' indexing year over the Average Wage Index for the year the earnings
#' were realized. The indexing year is the year two years prior to
#' the Normal Retirement Age (NRA) year. In other words, it is the
#' year a person turns 60. This function returns the Average Indexed
#' Monthly Earnings (AIME) for the highest 35 years of indexed
#' earnings after the year a person turns 21. Note the AIME calculation
#' always uses 35 years of earnings even if it means some years of
#' $0 earnings must be included. If the endYear is in the future,
#' estimates of FICA earnings and AWI are used and appropriate
#' warnings are issued.
#'
#' @param ssStmt the Social Security Statement as retrieved through read_xml_ssStatement()
#' @param endYear the last year to include in the calculation
#' @return The Average Indexed Monthly Earnings value
#' @examples
#' getAimeValue(ssStmt = JQPublicStatement, endYear = 2021)
#'
#' @export
getAimeValue <- function(ssStmt = NULL, endYear = NULL){

  # return calculatiom of AIME
  floor(sum(getAimeSet(ssStmt = ssStmt, endYear = endYear)$adjEarnings)/420)
}





#' The Primary Insurance Amount (PIA) is the actual Social Security
#' benefit. It is based on the average of the highest 35 years of
#' indexed earnings.
#'
#' @param piaBends the PIA bend points from \code{getPiaBendPts()}
#' @param aime The Average Indexed Monthly Value from \code{getAimeValue()}
#' @return The Primary Insurance Amount
#' @examples
#' getPia(piaBends = JQPublicBends, aime = JQPublicAime)
#'
#' @export
getPia <- function(piaBends = NULL, aime = NULL){

  floor(if(aime > piaBends$Second){
    .9 * piaBends$First +
      .32 * (piaBends$Second - piaBends$First) +
      .15 * (aime - piaBends$Second)
  } else if(aime > piaBends$First){
    .9 * piaBends$First +
      .32 * (aime - piaBends$First)
  } else {
    .9 * aime
  } * 10 / 10)
}





#' The Family Maximum Benefit is the maximum Social Security benefit
#' that can be paid based on an individual's earnings record. Like the
#' PIA, it is based on the average of the highest 35 years of indexed
#' earnings.
#'
#' @param maxPiaBends the family maximum PIA bend points from \code{getMaxPiaBendPts()}
#' @param aime The Average Indexed Monthly Value from \code{getAimeValue()}
#' @return The Family Maximum Benefit
#' @examples
#' getPia(piaBends = JQPublicBends, aime = JQPublicAime)
#'
#' @export
getFamilyMax <- function(maxPiaBends = NULL, aime = NULL){
  floor(
    if(aime > maxPiaBends$Third){
      1.5 * maxPiaBends$First +
        2.72 * (maxPiaBends$Second - maxPiaBends$First) +
        1.34 * (maxPiaBends$Third - maxPiaBends$Second) +
        1.75 * (aime - maxPiaBends$Third)
    } else if(aime > maxPiaBends$Second){
      1.5 * maxPiaBends$First +
        2.72 * (maxPiaBends$Second - maxPiaBends$First) +
        1.34 * (aime - maxPiaBends$Second)
    } else if(aime > maxPiaBends$First){
      1.5 * maxPiaBends$First +
        2.72 * (aime - maxPiaBends$First)
    } else {
      1.5 * aime
    } * 10 / 10)
}





# early retirement -
  # the PIA benefit is reduced 5/9 of one percent for each month
  # before normal retirement age, up to 36 months.
  # If the number of months exceeds 36, then the benefit is
  # further reduced 5/12 of one percent per month.
  # For example, if the number of reduction months is 60
  # then the benefit is reduced by 30 percent.
  # This maximum reduction is calculated as 36 months times 5/9 of 1 percent
  # plus 24 months times 5/12 of 1 percent.



# delayed retirement
  # credit is given according to a table of adjustments
  # (piaAdjustments$`Percent Yearly Credit`). These adjustment
  # values are given as yearly adjustments but applied by the
  # number of months delayed.




# COLA calculation



