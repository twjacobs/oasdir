## ----libraryOpts, echo=FALSE, message=FALSE, warning=FALSE---------------
library("knitr")
library("ggplot2")
library("scales")
library("lubridate")
library("reshape2")
library("earth")
library("oasdir")

opts_chunk$set(warning = FALSE,
               message = FALSE,
               echo = FALSE)

## ----NRAplot, echo = TRUE------------------------------------------------
ggplot(getFraSet(), aes(x = `Birth Year`, y = `Full Retirement Age`)) +
  geom_line() +
  theme_minimal()

## ----plotAwi, echo = TRUE------------------------------------------------
ggplot(getAwiSet(endYear = 2025), aes(x = Year, y = AWI, color = Status)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL)) +
  scale_y_continuous(labels = dollar_format()) 

## ----benefitBasePlot, echo=TRUE------------------------------------------
ggplot(getBenefitBasesSet(endYear = 2025), aes(x = Year, y = Amount,
                                               color = Status)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL)) +
  scale_y_continuous(labels = dollar_format())

## ----earnings, echo = TRUE, message=FALSE--------------------------------
# Read Social Security Statement
ssStatement <- system.file("extdata", "Early_Retirement_Sample.xml", package = "oasdir")
ssJQ <- readStatement(file = ssStatement)

# Retrieve some of the information from the statement
birthDateJQ <- as.Date(ssJQ$userInformation$dateOfBirth)
retireElgYearJQ <- year(birthDateJQ + years(62))
retireAgeJQ <-62
retireDateJQ <- birthDateJQ + years(retireAgeJQ) + months(1)

lastYear <- year(retireDateJQ)
earningsJQMedicare <- getMedicareEarnings(ssJQ, endYear = lastYear)
colnames(earningsJQMedicare) <- c("startYear", "endYear", "Earnings")
earningsJQMedicare$Type <- "Medicare"
earningsJQFica <- getFicaEarnings(ssJQ, endYear = lastYear)
colnames(earningsJQFica) <- c("startYear", "endYear", "Earnings")
earningsJQFica$Type <- "FICA"
earningsJQLong <- rbind(earningsJQMedicare, earningsJQFica)

## ----calcAwiIndexFactor, echo = TRUE-------------------------------------
aimeJQ <- getAimeValue(ssStmt = ssJQ, endYear = year(retireDateJQ))

## ----recordedBenefitsPlot, echo = TRUE-----------------------------------
# Construct a combined data set to plot
aimeSetJQ <- getAimeSet(ssStmt = ssJQ, endYear = year(retireDateJQ))
temp <- aimeSetJQ
temp$Type <- "Indexed"
colnames(temp) <- c("startYear", "endYear", "ficaEarnings", "factors", "Earnings", "Type")
earningsJQLong <- rbind(earningsJQLong, temp[, c("startYear", "endYear",
                                                 "Earnings", "Type")])
rm(temp)

# plot the data
ggplot(earningsJQLong, aes(x = `startYear`, y = Earnings)) +
  geom_line(aes(color = Type)) +
  geom_hline(yintercept = aimeJQ*12, linetype = 2) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format()) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL))

## ----bendPtPlot, echo = TRUE---------------------------------------------
plotdata <- melt(getPiaBendPtsSet(),
                 id.vars = c("Year", "Formula"),
                 variable.name = "Bend Point",
                 value.name = "Value")
ggplot(plotdata, aes(x = Year, y = Value,
                     color = Formula,
                     group = Formula)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Bend Point") +
  guides(colour = guide_legend(title.position = "top")) +
  facet_wrap(~ `Bend Point`)

## ----piaAmount, echo = TRUE----------------------------------------------
piaBendsJQ <- getPiaBendPts(year(retireDateJQ))

piaJQ <- getPia(piaBendsJQ, aimeJQ)

## ----maxBenefit, echo = TRUE---------------------------------------------
maxPiaBendsJQ <- getMaxPiaBendPts(year(retireDateJQ))

maxBenefitJQ <- getFamilyMax(maxPiaBends = maxPiaBendsJQ, aime = aimeJQ)

## ----piaAdjPlot, echo = TRUE---------------------------------------------
ggplot(data = getPiaAdjustments(birthYear = lubridate::year(birthDateJQ)),
       aes(x = `First Distribution Age`, y = `Percentage of PIA`, group = `Year of birth`)) +
  geom_line() +
  theme_minimal()

## ----cpiwPlot, echo = TRUE-----------------------------------------------
ggplot(getCpiwSet(), aes(x = Date, y = CPIW), color = Status) +
  geom_line() +
  theme_minimal()

## ----colaPlot------------------------------------------------------------
ggplot(getColaSet(), aes(x = Year, y = COLA)) +
  geom_line() +
  theme_minimal()

