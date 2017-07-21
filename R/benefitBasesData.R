

#' Return the \emph{Benefit Bases} data set.
#'
#' Each Social Security benefit is based on the history of earned and
#' taxable income for the beneficiary (or the beneficiaries spouse).
#' The income considered is limited by the Benefit and Contribution Base
#' which changes from year to year to reflect the average of wages
#' earned in that year. This amount is called the Taxed Social Security
#' Earnings and it is related to the Average Wage Index. The Social
#' Security web site contains a table of \href{https://www.ssa.gov/oact/COLA/cbb.html}{Contribution and Benefits Bases}.
#' This data set comes from there, but it is not a real time fetch, so
#' the data is current up to the time that this package was created. In
#' order to determine the Benefit Base for a year that is beyond the
#' last year in the Benefit Bases data, the data is fit with a model
#' which is used to project future Benefit Base values. If no endYear
#' is provided the function returns the data set of measured values
#' without any projected values.
#'
#' @param endYear the last year to include in the returned data set
#' @return the Benefit Bases data set
#' @examples
#' getBenefitBasesSet(endYear = 2025)
#' getBenefitBasesSet()
#'
#' @export
getBenefitBasesSet <- function(endYear = NULL){
  if(is.null(endYear)) {
    return(benefitBases)
  } else if(endYear < min(benefitBases$Year)){
    stop(paste("endYear is before earliest year in benefit bases data.",
               "No value was returned.\n",
               sep = "\n  "))
  }

  beginPrediction <- max(benefitBases$Year, na.rm = TRUE) + 1
  if(endYear <= beginPrediction) {
    return(benefitBases[benefitBases$Year <= endYear,])
  } else {
    benefitBasesChunk <- data.frame(Year = beginPrediction:endYear,
                                    Amount = predict(benefitBasesMars,
                                                     newdata = beginPrediction:endYear))
    benefitBasesChunk$Status = "Predicted"

    return(rbind(benefitBases, benefitBasesChunk))
  }
}





#' Return the \emph{Benefit Bases} values for input years.
#'
#' Return the Benefit Bases for input years. The values are either
#' measured or predicted through a model depending on whether or
#' not a measure value exists for the input year. Predicted values
#' will only be produced for years later than the last year in the
#' data set downloaded from the government site. No values for years
#' before the first year in the set will be predicted.
#'
#' @param year the years of the Benefit Base values to return
#' @return the Benefit Base values for \code{years}
#' @examples
#' getBenefitBaseValues(years = 2025)
#' getBenefitBaseValues(years = 2000:2025)
#'
#' @export
getBenefitBaseValues <- function(years = NULL) {
  if(is.null(years) || missing(years) || length(years) == 0) {
    stop(paste("Need to specify a year (or set of years) for which you",
               "want the Benefit Base value(s).\n",
               sep = "\n  "))

  } else if(max(years) < min(benefitBases$Year)) {
    stop(paste("All benefit base years requested are before the first year in the data set.",
               "No values were returned.\n",
               sep = "\n  "))

  } else if(min(years) < min(benefitBases$Year)) {
    warning(paste("The earliest benefit base year requested is before the first year in the data set.",
                  "The earliest year returned was the first year in the data set.\n",
                  sep = "\n  "))
    if(max(years) <= max(benefitBases$Year)) {
      c(benefitBases[benefitBases$Year %in% years,]$Amount)
    } else {
      warning("Benefit base values after ", max(benefitBases$Year), " are predicted.\n")
      c(benefitBases[benefitBases$Year %in% years,]$Amount,
        predict(benefitBasesMars, newdata = setdiff(years, benefitBases$Year)))
    }

  } else if(max(years) <= max(benefitBases$Year)) {
    benefitBases[benefitBases$Year %in% years,]$Amount

  } else if(min(years) <= max(benefitBases$Year)) {
    warning("Benefit base values after ", max(benefitBases$Year), " are predicted.\n")
    c(benefitBases[benefitBases$Year %in% years,]$Amount,
      predict(benefitBasesMars, newdata = setdiff(years, benefitBases$Year)))

  } else {
    warning("All benefit base values in this set are predicted.\n")
    c(predict(benefitBasesMars, newdata = years))
  }
}
