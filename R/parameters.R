############################
#
# -Separate out the different data set functions into their own files
# -Combine getSet and getValue functions into single vectorized functions
#
#
############################




#' Return the \emph{Comsumer Price Wage Index} data set.
#'
#' Legislation enacted in 1973 provides for cost-of-living adjustments,
#' or COLAs. With COLAs, Social Security and Supplemental Security Income
#' (SSI) benefits keep pace with inflation. The Social Security Act
#' specifies a formula for determining each COLA. According to the formula,
#' COLAs are based on increases in the
#' \href{https://www.ssa.gov/OACT/STATS/cpiw.html}{Consumer Price Index for Urban Wage Earners and Clerical Workers}
#' (CPI-W). CPI-Ws are calculated on a monthly basis by the Bureau of Labor
#' Statistics. This data set comes from the Social Security web site but it
#' is not a real time fetch, so the data is current up to the time that this
#' package was created. In order to determine the Consumer Price Index for a
#' year that is beyond the last year in the data, the data is fit with a
#' model which is used to project future CPIW values. If no endYear is
#' provided the function returns the data set of measured values without
#' any projected values.
#'
#' @param endYear the last year to include in the returned data set
#' @return the CPIW data set
#' @examples
#' getCpiwSet(endYear = 2025)
#' getCpiwSet()

#' @export
getCpiwSet <- function(endYear = NULL){
  if(is.null(endYear)) {
    return(cpiw)
  }

  endDate <- lubridate::make_date(year = endYear, month = 12)
  beginDate <- lubridate::add_with_rollback(max(cpiw$Date, na.rm = TRUE),
                                            lubridate::as.period(1, unit = "month"))
  if(endDate <= beginDate) {
    return(cpiw[cpiw$Date <= endDate, ])
  } else {
    # Convert Dec of endYear to months since Jan 1974
    # (the model was constructed using months since 1974)
    endMonth <- (endYear - 1973) * 12 - 1
    # Create chunk from max of cpiw$Date to Dec of endYear
    cpiwChunk <- data.frame(Date = seq(from = beginDate, to = endDate, by = "month"),
                            CPIW = predict(cpiwMars, newdata = (nrow(cpiw)):endMonth),
                            Status = "Predicted")
    return(rbind(cpiw, cpiwChunk))
  }
}




#' Return the \emph{Cost of Living Adjustment} data set.
#'
#' Legislation enacted in 1973 provides for cost-of-living adjustments,
#' or COLAs. With COLAs, Social Security and Supplemental Security Income
#' (SSI) benefits keep pace with inflation. The Social Security Act specifies
#' a formula for determining each COLA. According to the formula, COLAs are
#' based on increases in the Consumer Price Index for Urban Wage Earners and
#' Clerical Workers (CPI-W). CPI-Ws are calculated on a monthly basis by the
#' Bureau of Labor Statistics. A COLA effective for December of the current
#' year is equal to the percentage increase (if any) in the average CPI-W for
#' the third quarter of the current year over the average for the third quarter
#' of the last year in which a COLA became effective. If there is an increase,
#' it must be rounded to the nearest tenth of one percent. If there is no
#' increase, or if the rounded increase is zero, there is no COLA. For an
#' example computation of COLA, see the Lastest
#' \href{https://www.ssa.gov/oact/COLA/latestCOLA.html}{Cost of Living Adjustment}
#' page on the Social Security web site.
#'
#' @param endYear the last year to include in the returned data set
#' @return the cost of living adjustment data set
#' @examples
#' getColaSet(endYear = 2025)
#' getColaSet()
#'
#' @export
getColaSet <- function(endYear = NULL) {
  if(is.null(endYear)) {
    return(cola)
  }

  beginYear <- max(cola$Year, na.rm = TRUE) + 1
  if(endYear <= beginYear) {
    return(cola[cola$Year <= endYear,])
  } else {
    # This doesn't do anything yet. Need to put in function
    # to calculate COLAs based on CPIW
    colaChunk <- data.frame(Year = beginYear:endYear,
                                    COLA = NA)
    colaChunk$Status = "Predicted"

    return(rbind(cola, colaChunk))
  }
}




#' Return the \emph{Primary Insurance Amount Bend Points} data set.
#'
#' The Primary Insurance Amount (PIA) is the base value of a worker's OASDI
#' at full retirement age. It is calculated by summing three separate percentages
#' of portions of the worker's Average Indexed Monthly earnings (AIME). The
#' portions depend on the year in which a worker attains age 62, becomes
#' disabled before age 62, or dies before attaining age 62. The threshold
#' points that determine the portions of a worker's AIME that apply to each
#' percentage are called the PIA bend points. PIA bend points are based on
#' the Average Wage Index (AWI), using the AWI for 1977 as the reference year.
#' The Social Security web site shows \href{https://www.ssa.gov/oact/COLA/piaformula.html}{an example}
#' of how to calculate these points.
#'
#' @param endYear the last year to include in the returned data set
#' @return the primary insurance amount bend point data set
#' @examples
#' getPiaBendPtsSet(endYear = 2025)
#' getPiaBendPtsSet()
#'
#' @export
getPiaBendPtsSet <- function(endYear = NULL) {
  if(is.null(endYear)) {
    return(piaBends)
  }

  beginYear <- max(piaBends$Year, na.rm = TRUE) + 1
  if(endYear <= beginYear) {
    return(piaBends[piaBends$Year <= endYear, ])
  } else {
    awiTemp <- getAwiSet(endYear = endYear)
    # Calculate missing bend points using bend point
    # formula
    futurePiaBends <-
      do.call(rbind, lapply((max(piaBends$Year)+1):endYear,
                            FUN = function(y){
                              data.frame(Year = y,
                                         First =
                                           round(awiTemp[awiTemp$Year == y-2,]$AWI /
                                                   awiTemp[awiTemp$Year == 1977,]$AWI *
                                                   piaBends[piaBends$Year == 1979,]$First),
                                         Second =
                                           round(awiTemp[awiTemp$Year == y-2,]$AWI /
                                                   awiTemp[awiTemp$Year == 1977,]$AWI *
                                                   piaBends[piaBends$Year == 1979,]$Second),
                                         Third =
                                           round(awiTemp[awiTemp$Year == y-2,]$AWI /
                                                   awiTemp[awiTemp$Year == 1977,]$AWI *
                                                   piaBends[piaBends$Year == 1979,]$Third),
                                         Formula = c("PIA", "Maximum"))
                            }))
    piab <- rbind(piaBends, futurePiaBends)
    piab <- piab[order(piab$Formula, piab$Year), ]

    return(piab)
  }
}



#' Return the \emph{Primary Insurance Amount Bend Points} for a year.
#'
#' Return the PIA bend points for a year. These values are
#' calculated using AWI values.
#'
#' @param year the year for the PIA bend points to return
#' @return the PIA bend points for \code{year}
#' @examples
#' getPiaBendPts(year = 2025)
#'
#' @export
getPiaBendPts <- function(year = NULL) {
  if(is.null(year) || missing(year)) {
    stop("Need to specify the year for which you want the PIA bend points.")
  } else if(year < max(piaBends$Year)) {
    piaBends[which(piaBends$Year == year & piaBends$Formula == "PIA"),
             c("First", "Second")]
  } else {
    awiTemp <- getAwiSet(endYear = year)
    data.frame(First =
                 round(awiTemp[awiTemp$Year == year - 2,]$AWI /
                         awiTemp[awiTemp$Year == 1977,]$AWI *
                         piaBends[piaBends$Year == 1979 &
                                  piaBends$Formula == "PIA",]$First),
               Second =
                 round(awiTemp[awiTemp$Year == year - 2,]$AWI /
                         awiTemp[awiTemp$Year == 1977,]$AWI *
                         piaBends[piaBends$Year == 1979 &
                                  piaBends$Formula == "PIA",]$Second))
  }
}



#' Return the maximum family benefit \emph{Primary Insurance Amount Bend Points} for a year.
#'
#' Return the family maximum PIA bend points for a year. These values are
#' calculated using AWI values.
#'
#' @param year the year for the family maximum PIA bend points to return
#' @return the family maximum PIA bend points for \code{year}
#' @examples
#' getMaxPiaBendPts(year = 2025)
#'
#' @export
getMaxPiaBendPts <- function(year = NULL) {
  if(is.null(year) || missing(year)) {
    stop("Need to specify the year for which you want the family maximum PIA bend points.")
  } else if(year < max(piaBends$Year)) {
    piaBends[which(piaBends$Year == year & piaBends$Formula == "Maximum"),
             c("First", "Second", "Third")]
  } else {
    awiTemp <- getAwiSet(endYear = year)
    data.frame(First =
                 round(awiTemp[awiTemp$Year == year - 2,]$AWI /
                         awiTemp[awiTemp$Year == 1977,]$AWI *
                         piaBends[piaBends$Year == 1979 &
                                    piaBends$Formula == "Maximum",]$First),
               Second =
                 round(awiTemp[awiTemp$Year == year - 2,]$AWI /
                         awiTemp[awiTemp$Year == 1977,]$AWI *
                         piaBends[piaBends$Year == 1979 &
                                    piaBends$Formula == "Maximum",]$Second),
               Third =
                 round(awiTemp[awiTemp$Year == year - 2,]$AWI /
                         awiTemp[awiTemp$Year == 1977,]$AWI *
                         piaBends[piaBends$Year == 1979 &
                                    piaBends$Formula == "Maximum",]$Third))
  }
}




#' Return the \emph{Primary Insurance Amount Adjustments} data set.
#'
#' If a worker takes benefit distributions before or after the
#' normal retirement age, the PIA is adjusted by multiplying it
#' by a percentage.
#' A \href{https://www.ssa.gov/oact/ProgData/ar_drc.html}{table of adjustments}
#' is available on the Social Security web site. Any year of birth greater
#' than 1960 uses the same percentage adjustments as for 1960. Therefore
#' any year passed into the function greater than 1960 will return the
#' same data set.
#'
#' @param birthYear the last birth year to include
#' @return the percentage adjustment data set for all ages up to the birth year
#' @examples
#' getPiaAdjustmentSet(endYear = 1955)
#' getPiaAdjustmentSet()
#'
#' @export
getPiaAdjustmentsSet <- function(endYear = NULL) {
  if(is.null(endYear)) {
    return(piaAdjustments)
  }

  lastYear <- max(piaAdjustments$`Year of birth`, na.rm = TRUE) + 1
  if(endYear <= lastYear) {
    return(piaAdjustments[piaAdjustments$`Year of birth` <= endYear,])
  } else {
    return(piaAdjustments)
  }
}



#' Return the \emph{Primary Insurance Amount Adjustments} for a birth year.
#'
#' If a worker takes benefit distributions before or after the
#' normal retirement age, the PIA is adjusted by multiplying it
#' by a percentage.
#' A \href{https://www.ssa.gov/oact/ProgData/ar_drc.html}{table of adjustments}
#' is available on the Social Security web site. This function returns
#' the adjustments listed in the table for the input birth year.
#'
#' @param birthYear the birth year of the worker
#' @return the percentage adjustment data set for the input birth year
#' @examples
#' getPiaAdjustmentSet(birthYear = 1955)
#'
#' @export
getPiaAdjustments <- function(birthYear = NULL) {
  if(is.null(birthYear) || missing(birthYear)) {
    stop("Need to specify the birth year for which you want the primary insurance amount adjustments.\n")
  } else if(birthYear < min(piaAdjustments$`Year of birth`)) {
    stop(paste("Input birth year is before the first available year in the data set.",
               "No data was returned\n",
               sep = "\n  "))
  }

  if(birthYear <= max(piaAdjustments$`Year of birth`)) {
    return(piaAdjustments[piaAdjustments$`Year of birth` == birthYear,])
  } else {
    return(piaAdjustments[piaAdjustments$`Year of birth` == 1960,])
  }
}




