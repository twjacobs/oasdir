
#' Return the \emph{National Average Wage Index} data set.
#'
#' The \emph{National Average Wage Index} is used to normalize wages across
#' different years. Wage indexing depends on the year in which a person is
#' first elgible to receive OASDI benefits. The year of first elgibility for
#' OASDI is at age 62. An individuals earnings are always indexed to the average
#' wage level two years prior to the year of first eligibility. This is called
#' the indexing year. Thus, for a person reaching age 62 in 2017, the person’s
#' earnings would be indexed to the average wage index for 2015, which is the
#' indexing year. A table of \href{https://www.ssa.gov/OACT/COLA/awiseries.html}{Average Wage Indexes}
#' is available on the Social Security web site. This data set comes from there,
#' but it is not a real time fetch, so the data is current up to the time that
#' this package was created. In order to determine the Average Indexed Monthly
#' Earnings when the indexing year is beyond the last year in the AWI data, the
#' data is fit with a model which is used to project future wage index values.
#' If no endYear is provided the function returns the data set of measured values
#' without any projected values.
#'
#' @param endYear the last year to include in the returned data set
#' @return the AWI data set
#' @examples
#' getAwiSet(endYear = 2025)
#' getAwiSet()
#'
#' @export
getAwiSet <- function(endYear = NULL) {
  if(is.null(endYear)) {
    return(awi)
  }

  if(endYear < min(awi$Year)) {
    stop(paste("endYear parameter is before the first year of AWI data set.",
               "There are no values to return.",
               sep = "\n  "))
  }

  beginPrediction <- max(awi$Year, na.rm = TRUE) + 1
  if(endYear <= beginPrediction) {
    return(awi[awi$Year <= endYear,])
  } else {
    awiChunk <- data.frame(Year = beginPrediction:endYear,
                           AWI = predict(awiMars, newdata = beginPrediction:endYear))
    awiChunk$`Annual change` = c((awiChunk$AWI[1] - awi$AWI[nrow(awi)]) /
                                   awi$AWI[nrow(awi)],
                                 (awiChunk$AWI[2:(nrow(awiChunk))] -
                                    awiChunk$AWI[1:(nrow(awiChunk) - 1)]) /
                                   awiChunk$AWI[1:(nrow(awiChunk) - 1)])
    awiChunk$Status = "Predicted"

    return(rbind(awi, awiChunk))
  }
}



#' Return the \emph{National Average Wage Index} values for input years.
#'
#' Return the AWI values for input years. The values are either
#' measured or predicted through a model depending on whether or
#' not a measure value exists for the input year. Predicted values
#' will only be produced for years later than the last year in the
#' data set downloaded from the government site. No values for years
#' before the first year in the set will be predicted.
#'
#' @param year the years fof the AWI values to return
#' @return the AWI values for \code{years}
#' @examples
#' getAwiValues(years = 2025)
#' getAwiValues(years = 2000:2025)
#'
#' @export
getAwiValues <- function(years = NULL) {
  if(is.null(years) || missing(years) || length(years) == 0) {
    stop(paste("Need to specify a year (or set of years) for which you",
               "want AWI value(s).\n",
               sep = "\n  "))

  } else if(max(years) < min(awi$Year)) {
    stop(paste("All years requested are before the first year in the data set.",
               "No values can be returned.\n",
               sep = "\n  "))

  } else if(min(years) < min(awi$Year)) {
    warning(paste("In getAwiValues: ",
                  "The earliest year requested is before the first year in the data set.",
                  "The earliest year returned was the first year in the data set.\n",
                  sep = "\n  "))
    if(max(years) <= max(awi$Year)) {
      c(awi[awi$Year %in% years,]$AWI)
    } else {
      warning("In getAwiValues: Values after ", max(awi$Year), " are predicted.\n")
      c(awi[awi$Year %in% years,]$AWI,
        predict(awiMars, newdata = setdiff(years, awi$Year)))
    }

  } else if(max(years) <= max(awi$Year)) {
    awi[awi$Year %in% years,]$AWI

  } else if(min(years) <= max(awi$Year)) {
    warning("In getAwiValues: Values after ", max(awi$Year), " are predicted.\n")
    c(awi[awi$Year %in% years,]$AWI,
      predict(awiMars, newdata = setdiff(years, awi$Year)))

  } else {
    warning("in getAwiValues: All values in the returned set are predicted.\n")
    c(predict(awiMars, newdata = years))
  }
}



