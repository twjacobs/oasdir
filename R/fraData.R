

#' Return the \emph{Full Retirement Age} data set
#'
#' A worker's Full Retirement Age is the age at which the worker
#' is entitled to the full Primary Insurance Amount (PIA). If a
#' worker starts taking distributions before the full retirement
#' age, the distributions are reduced. Conversely, if a worker
#' delays distributions after the full retirement age, the
#' distributions are more.
#'
#' @param endYear the last year to include in the returned data set
#' @return the full retirement age data set
#' @examples
#' getFraSet(endYear = 2025)
#' getFraSet()
#'
#' @export
getFraSet <- function(endYear = NULL){
  if(is.null(endYear)) {
    return(fra)
  } else if(endYear < min(fra$`Birth Year`)){
    stop("endYear is before earliest birth year in full retirement age data. No age was returned.")
  }

  # Full retirement age for any birth year after the last year of the full retirement
  # age data set is 67, so prediction of FRA is just the constant, 67.
  beginPrediction <- max(fra$`Birth Year`, na.rm = TRUE) + 1
  if(endYear <= beginPrediction) {
    return(fra[fra$`Birth Year` <= endYear,])
  } else {
    fraChunk <- data.frame(`Birth Year` = beginPrediction:endYear,
                           `Full Retirement Age` = 67,
                           check.names = FALSE)

    return(rbind(fra, fraChunk))
  }
}






#' Return the \emph{Full Retirement Age} values for input years.
#'
#' Return the Full Retirement Age for input years. No values for years
#' before the first year in the set will be predicted.
#'
#' @param year the years of the Full Retirement Age to return
#' @return the Full Retirement Age for \code{years}
#' @examples
#' getFraValues(years = 2025)
#' getFraValues(years = 2000:2025)
#'
#' @export
getFraValues <- function(years = NULL) {
  if(is.null(years) || missing(years) || length(years) == 0) {
    stop("Need to specify a year (or set of years) for which you\n  want the Full Retirement Age(s).")

  } else if(max(years) < min(fra$`Birth Year`)) {
    stop("All birth years requested are before the first year in the data set.\n  No ages were be returned.")

  } else if(min(years) < min(fra$`Birth Year`)) {
    warning("The earliest birth year requested is before the first year in the data set.\n  The earliest age returned will be the first age in the data set.")
    if(max(years) <= max(fra$`Birth Year`)) {
      c(fra[fra$`Birth Year` %in% years,]$`Full Retirement Age`)
    } else {
      warning("Full Retirement ages after ", max(fra$`Birth Year`), " are predicted.")
      c(fra[fra$`Birth Year` %in% years,]$`Full Retirement Age`,
        rep(67, times = length(setdiff(years, fra$`Birth Year`))))
    }

  } else if(max(years) <= max(fra$`Birth Year`)) {
    fra[fra$`Birth Year` %in% years,]$Amount

  } else if(min(years) <= max(fra$`Birth Year`)) {
    warning("Full Retirement ages after ", max(fra$`Birth Year`), " are predicted.")
    c(fra[fra$`Birth Year` %in% years,]$`Full Retirement Age`,
      rep(67, times = length(setdiff(years, fra$`Birth Year`))))

  } else {
    warning("All Full Retirement ages in this set are predicted.")
    rep(67, times = length(years))
  }
}
