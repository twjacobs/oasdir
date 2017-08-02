
#' Return a individual's retirement vitals.
#'
#' Social Secuity Benefit (normal retirement) are calculated based on AIME using a specific
#' retirement age (actually the age when benefits are started). Several retirement related variables
#' are calculated and returned in a named list. This data can be used to calculate estimated
#' primary insurance amount (PIA) if benefits begin at various ages (See getPIAData in this package).
#'
#' @param ssStmt the Social Security Statement as retrieved through read_xml_ssStatement()
#' @param retireAge the age of reciepient when earnings end. If no age is given, full retirement age (FRA) will be used.
#' @return Named list of several retirement related values including estimated benefit beginning at various ages
#'
#' @export
getRetirementData <- function(ssStmt = NULL, retireAge = NULL) {
  earner <- ssStmt$userInformation$name
  birthDate <- as.Date(ssStmt$userInformation$dateOfBirth)
  retireElgYear <- lubridate::year(birthDate + years(62))
  fra <- ssStmt$userInformation$fullRetirementAge
  if(is.null(retireAge)) {
    retireAge <- ssStmt$userInformation$fullRetirementAge
  }
  retireDate <- birthDate + months(round(retireAge*12 + 1))
  retireYear <- year(retireDate)
  earningsMedicare <- getMedicareEarnings(ssStmt, endYear = retireYear)
  colnames(earningsMedicare) <- c("startYear", "endYear", "Earnings")
  earningsMedicare$Type <- "Medicare"
  earningsFica <- getFicaEarnings(ssStmt, endYear = retireYear)
  colnames(earningsFica) <- c("startYear", "endYear", "Earnings")
  earningsFica$Type <- "FICA"
  aimeSet <- getAimeSet(ssStmt, endYear = retireYear)
  aimeSet$Type <- "Indexed"
  colnames(aimeSet) <- c("startYear", "endYear", "ficaEarnings", "factors", "Earnings", "Type")
  earnings <- rbind(earningsMedicare, earningsFica)
  remove(earningsMedicare, earningsFica)
  aime <- getAimeValue(ssStmt, endYear = retireYear)
  list(earner=earner, birthDate=birthDate, fra=fra, retireElgYear=retireElgYear, retireAge=retireAge,
       retireDate=retireDate, retireYear=retireYear, earnings=earnings, aime=aime, aimeSet=aimeSet)
}


#' Return estimates individual's retirement benefits.
#'
#' Provides an estimate of the monthly primary insurance amount (PIA) for benefits beginning at several ages
#' based on data from a Social Security Statement (normal retirement) are calculated based on AIME using a specific
#' retirement age (actually the age when benefits are started). Several retirement related variables
#' are calculated and returned in a named list. This data can be used to calculate estimated
#' primary insurance amount (PIA) if benefits begin at various ages (See getPIAData in this package).
#'
#' @param retDate list of retirement related values as retrieved through getRetirementData
#' @param ssStmt the Social Security Statement as retrieved through read_xml_ssStatement()
#' @param retireAge the age of reciepient when earnings end. If no age is given, full retirement age (FRA) will be used.
#' @return Named list of several retirement related values including estimated benefit beginning at various ages
#'
#' @export
getPIAData <- function(retData = NULL, ssStmt = NULL, retireAge = NULL) {
  # The PIA benefit is reduced 5/9 of one percent for each month before normal
  # retirement age, up to 36 months. For months earlier than 36, the benefit
  # is reduced 5/12 of one percent per month.
  # The PIA benefit is increased by a percentage that is determined
  # by your birth year(piaAdjustments$`Percent Yearly Credit`). These adjustment
  # values are given as yearly adjustments but applied by the number of months delayed.

  #  Determine the PIA at full retirement age
  piaBends <- getPiaBendPts(retData$retireYear)
  pia <- getPia(piaBends, retData$aime)
  maxPiaBends <- getMaxPiaBendPts(retData$retireYear)
  piaAdjustments <- getPiaAdjustments(birthYear = year(retData$birthDate))

  piaDf <- data.frame(firstDistributionAge = 62:70)
  piaDf$yearlyBenefit <- pia * 12
  piaDf$monthsEarly <- NA
  piaDf$monthsLate <- NA

  early <- which(piaDf$firstDistributionAge < retData$fra)
  late <- which(piaDf$firstDistributionAge > retData$fra)

  piaDf$monthsEarly[early] <- (retData$fra - piaDf$firstDistributionAge[early]) * 12
  piaDf$monthsLate[late] <- (piaDf$firstDistributionAge[late] - retData$fra) * 12

  veryEarly <- which(piaDf$monthsEarly > 36)
  early <- which(piaDf$monthsEarly <= 36)

  if(length(veryEarly) > 0){
    piaDf$yearlyBenefit[veryEarly] <- piaDf$yearlyBenefit[veryEarly] *
      (100 - (5*36/9 + 5*(piaDf$monthsEarly[veryEarly] - 36)/12))/100
  }

  if(length(early) > 0){
    piaDf$yearlyBenefit[early] <- piaDf$yearlyBenefit[early] *
      (100 - 5*piaDf$monthsEarly[early]/9)/100
  }

  if(length(late) > 0) {
    # I use min here but all values should be the same  #
    creditPercent <- min(piaAdjustments$`Percent Yearly Credit`)
    piaDf$yearlyBenefit[late] <- piaDf$yearlyBenefit[late] *
      (1 + (creditPercent*piaDf$monthsLate[late]/1200))
  }

  piaDf$date <- as.period(piaDf$firstDistributionAge, unit = "year") +
    retData$birthDate +
    as.period(1, unit = "month")

  list(piaBends=piaBends, pia=pia, maxPiaBends=maxPiaBends, piaAdjustments=piaAdjustments,
       piaDf=piaDf)
}
