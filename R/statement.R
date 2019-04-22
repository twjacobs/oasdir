
#' Return Social Security information from an XML file.
#'
#' Your Social Security statement is available on the Social
#' Security web site as a PDF file and an XML file. This
#' function reads the XML file and returns a list containing
#' all the information the XML file provides. First download
#' the XML file you your computer then provide the file name
#' as an input to this function. Use either the full path name
#' or a relative path name as part of the file name input.
#'
#' @param file the name of the XML file to read
#' @return a list containing the Social Security Statement information
#' @examples
#' read_xml_ssStatement(file = "./myStatementData/richieRich.xml")
#'
#' @export
readStatement <- function(file = NULL) {
  if(is.null(file) || missing(file)) {
    stop("Need to specify the Social Security XML file to read.")
  } else if (!file.exists(file)) {
    stop(paste("file", file, "could not be found. Did you pass in the whole path?"))
  } else {
    ssData <- xml2::read_xml(file)
    earningsRecord <- xml2::xml_child(ssData, ".//osss:EarningsRecord")
    userInformation <- xml2::xml_child(ssData, ".//osss:UserInformation")
    estimatedBenefits <- xml2::xml_child(ssData, ".//osss:EstimatedBenefits")
    retirementAge <- xml2::xml_find_all(estimatedBenefits, ".//osss:RetirementAge")

    # The retirement (old age) data requires some munging, so we
    # do it outside the list creation then add it in
    oaYears <- xml2::xml_integer(xml2::xml_child(retirementAge, ".//osss:Years"))
    oaMonths <- xml2::xml_integer(xml2::xml_child(retirementAge, ".//osss:Months"))
    oaMonths[is.na(oaMonths)] <- 0
    oaYears <- oaYears + oaMonths/12

    # Full retirement age can contain 1 or 2 fields (years and possibly months)
    if(xml2::xml_length(xml2::xml_child(xml2::xml_child(estimatedBenefits,
                                                        ".//osss:FullRetirementEstimate"),
                                        "./osss:RetirementAge")) == 1) {
      fullRetirementAge <-
        xml2::xml_integer(xml2::xml_child(xml2::xml_child(estimatedBenefits,
                                                          ".//osss:FullRetirementEstimate"),
                                          ".//osss:Years"))
    } else {
      fullRetirementAge <-
        xml2::xml_integer(xml2::xml_child(xml2::xml_child(estimatedBenefits,
                                                          ".//osss:FullRetirementEstimate"),
                                          ".//osss:Years")) +
        xml2::xml_integer(xml2::xml_child(xml2::xml_child(estimatedBenefits,
                                                          ".//osss:FullRetirementEstimate"),
                                          ".//osss:Months")) / 12
    }

    # The earnings data can have negative values (the sample XML data
    # has -1). So create the data frame here and zero out negatives
    earnings <- data.frame(
      startYear = as.integer(xml2::xml_attr(xml2::xml_find_all(earningsRecord,
                                                               ".//osss:Earnings"), "startYear")),
      endYear = as.integer(xml2::xml_attr(xml2::xml_find_all(earningsRecord,
                                                             ".//osss:Earnings"), "endYear")),
      ficaEarnings = xml2::xml_integer(xml2::xml_find_all(earningsRecord, ".//osss:FicaEarnings")),
      medicareEarnings = xml2::xml_integer(xml2::xml_find_all(earningsRecord, ".//osss:MedicareEarnings"))
    )
    if(length(which(earnings$ficaEarnings < 0)) > 0){
      earnings[which(earnings$ficaEarnings < 0),]$ficaEarnings <- 0
      earnings[which(earnings$medicareEarnings < 0),]$medicareEarnings <- 0
    }

    list(
      creationDate =
        lubridate::ymd_hms(xml2::xml_text(xml2::xml_find_all(ssData, ".//osss:FileCreationDate"))),
      userInformation = list(
        name = xml2::xml_text(xml2::xml_child(userInformation, ".//osss:Name")),
        dateOfBirth = as.Date(xml2::xml_text(xml2::xml_child(userInformation, ".//osss:DateOfBirth"))),
        fullRetirementAge = fullRetirementAge
      ),
      earnings = earnings,
      oldAge = data.frame(
        age = oaYears,
        estimate = xml2::xml_integer(xml2::xml_find_all(estimatedBenefits, ".//osss:Estimate"))
      ),
      survivorBenefits = list(
        oneTimeDeath =
          xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:OneTimeDeathBenefit")),
        survivorsEstimateChild =
          xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:SurvivorsEstimateChild")),
        survivorsEstimateFamily =
          xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:SurvivorsEstimateFamily")),
        survivorsEstimateRetired =
          xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:SurvivorsEstimateRetired")),
        survivorsEstimateSpouseChild =
          xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:SurvivorsEstimateSpouseChild"))
      ),
      disabilityEstimate =
        xml2::xml_integer(xml2::xml_child(estimatedBenefits, ".//osss:DisabilityEstimate")),
      ficaTaxTotal = list(
        Employer = xml2::xml_integer(xml2::xml_child(earningsRecord,
                                         ".//osss:FicaTaxTotalEmployer")),
        Individual = xml2::xml_integer(xml2::xml_child(earningsRecord,
                                           ".//osss:FicaTaxTotalIndividual"))
      ),
      medicareTaxTotal = list(
        Employer = xml2::xml_integer(xml2::xml_child(earningsRecord,
                                         ".//osss:MedicareTaxTotalEmployer")),
        Individual = xml2::xml_integer(xml2::xml_child(earningsRecord,
                                           ".//osss:MedicareTaxTotalIndividual"))
      )
    )
  }
}




#' Return \emph{FICA} earnings data.
#'
#' Taxes under the Federal Insurance Contributions Act (FICA) are
#' composed of the old-age, survivors, and disability insurance taxes,
#' also known as social security taxes, and the hospital insurance tax,
#' also known as Medicare taxes. Different rates apply for these taxes.
#' The amount an employee pays in FICA tax is capped at the Benefit and
#' Contribution Base which can change from year to year according to
#' the Average Wage Index (AWI). The data set returned will contain the
#' actual values for years that the government listed at the time
#' this package was built. For any year beyond that, an estimate is
#' returned based on estimated values for the Benefit and Contribution
#' Base and full wage earnings. Since Medicare taxes are not capped
#' the full wage earnings are estimated using the medicare earnings
#' (they are the same). The minimum of estimated Medicare earnings
#' and estimated Benefit and Contribution Base will be the FICA earnings
#' returned.
#'
#' The \code{endYear} is the last year of FICA earnings to return. If no
#' \code{endYear} is given, only the FICA earnings contained in the input
#' Social Security Statement object are returned.
#'
#' @param endYear the last year to include in the returned data set
#' @param statement the Social Security Statement Object returned from \code{readStatement}
#' @param predict if TRUE use a MARS model to predict future wage earnings, otherwise just replicate the last known value
#' @return the FICA earnings data set
#' @examples
#' getFicaEarnings(statement = myStatement, endYear = 2025)
#' getFicaEarnings(statement = myStatement)

#' @export
getFicaEarnings <- function(statement = NULL, endYear = NULL, predict = TRUE) {
  if(is.null(statement) || missing(statement)){
    stop(paste("Parameter 'statement' is not optional.",
               "Use 'readStatement' to get a statement object and pass that into this function.\n",
               sep = "\n  "))
  }

  if(is.null(endYear)) {
    return(statement$earnings[, c("startYear", "endYear", "ficaEarnings")])
  }

  beginPrediction <- max(statement$earnings$endYear)
  if(endYear <= beginPrediction) {
    return(statement$earnings[, c("startYear", "endYear", "ficaEarnings")])

  } else {
    medicareEarnings <- getMedicareEarnings(statement, endYear = endYear, predict = predict)$medicareEarnings
    benefitBaseValues <- getBenefitBaseValues(min(statement$earnings$startYear):endYear)

    return(data.frame(startYear = min(statement$earnings$startYear):endYear,
                      endYear = min(statement$earnings$startYear):endYear,
                      ficaEarnings = pmin(medicareEarnings, benefitBaseValues)))
  }
}




#' Return the \emph{Medicare} earnings data set.
#'
#' Taxes under the Federal Insurance Contributions Act (FICA) are
#' composed of the old-age, survivors, and disability insurance taxes,
#' also known as social security taxes, and the hospital insurance tax,
#' also known as Medicare taxes. Different rates apply for these taxes.
#' The amount an employee pays in FICA tax is capped at the Benefit and
#' Contribution Base which can change from year to year according to
#' the Average Wage Index (AWI). The data set returned will contain the
#' actual values for years that the government listed at the time
#' this package was built. For any year beyond that, an estimate is
#' returned.
#'
#' The \code{endYear} is the last year of Medicare earnings to return. If no
#' \code{endYear} is given, only the Medicare earnings contained in the input
#' Social Security Statement object are returned.
#'
#' @param endYear the last year to include in the returned data set
#' @param statement the Social Security Statement Object returned from \code{readStatement}
#' @param predict if TRUE, use a MARS model to predict future medicare earnings, else replicate the last know value
#' @return the Medicare earnings data set
#' @examples
#' getMedicareEarnings(statement = myStatement, endYear = 2025)
#' getMedicareEarnings(statement = myStatement)

#' @export
getMedicareEarnings <- function(statement = NULL, endYear = NULL, predict = TRUE) {
  if(is.null(statement) || missing(statement)){
    stop(paste("parameter 'statement' is not optional.",
               "Use 'readStatement' to get a statement object and pass that into this function.\n",
               sep = "\n  "))
  }

  if(is.null(endYear)) {
    return(statement$earnings[, c("startYear", "endYear", "medicareEarnings")])
  }

  beginPrediction <- max(statement$earnings$endYear) + 1
  if(endYear < beginPrediction) {
    return(statement$earnings[, c("startYear", "endYear", "medicareEarnings")])

  } else {
    warning("Medicare earnings starting with year ", beginPrediction, " are predicted.\n")
    medicareMars <- earth::earth(medicareEarnings ~ startYear,
                                 statement$earnings)
    if(predict) {
      medicareEarnings = predict(medicareMars, newdata = beginPrediction:endYear)
    } else {
      medicareEarnings = rep(statement$earnings$medicareEarnings[nrow(statement$earnings)],
                             times = endYear - beginPrediction + 1)
    }
    if(length(which(medicareEarnings < 0)) > 0){
      medicareEarnings[which(medicareEarnings < 0)] <- 0
    }


    # Create data frame of predicted earnings
    medicareChunk <- data.frame(startYear = beginPrediction:endYear,
                            endYear = beginPrediction:endYear,
                            medicareEarnings = medicareEarnings)
    # and return the combined measured and predicted vaues
    return(rbind(statement$earnings[, c("startYear", "endYear", "medicareEarnings")],
                 medicareChunk))
  }
}
