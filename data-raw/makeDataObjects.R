
library("reshape2")
library("devtools")
library("readODS")
library("rprojroot")

makeDataObjects <- function(){

# National Average Wage Index Data
  awi <- read_ods("./Average Wage Index Series.ods",
                  sheet = 1,
                  col_names = TRUE)
  awi$`Annual change` <- c(NA, (awi$AWI[2:(nrow(awi))] - awi$AWI[1:(nrow(awi) - 1)]) /
                             awi$AWI[1:(nrow(awi) - 1)])
  awi$Status <- "Actual"


# awi model
  awiMars <- earth::earth(AWI ~ Year, awi)




# Contribution and Benefit Bases
  benefitBases <- read_ods("./Contribution and Benefit Bases.ods",
                           sheet = 1,
                           col_names = TRUE)
  benefitBases$Status <- "Actual"

# benefit bases model
  benefitBasesMars <- earth::earth(Amount ~ Year, benefitBases)




# Primary Insurance Amount Bend Points and
  # Maximum Family Amount Bend Points
  piaBends <- read_ods("./PIA Bend Points.ods",
                       sheet = 1,
                       col_names = TRUE)



  # Primary Insurance Amount Early/Late Adjustments
  piaAdjustments <- read_ods("./PIA Adjustment.ods",
                             sheet = 1,
                             col_names = TRUE)




# Consumer Price Index for Urban Wage Earners and Clerical Workers
  cpiw <- read_ods("./CPIW.ods",
                   sheet = 1,
                   col_names = TRUE)
  cpiw <- reshape2::melt(cpiw,
                         id.vars = "Year",
                         variable.name = "Month",
                         value.name = "CPIW")
  cpiw$Date <- lubridate::make_date(year = cpiw$Year, month = cpiw$Month)
  cpiw$Year <- NULL
  cpiw$Month <- NULL
  cpiw <- cpiw[order(cpiw$Date), ]
  cpiw <- cpiw[!is.na(cpiw$CPIW),]
  cpiw$Status <- "Actual"

# CPIW model
  # Create a sequence of months since 1974 for regression
  cpiw$m <- 0:(nrow(cpiw) - 1)
  # Run regression on this sequence
  cpiwMars <- earth::earth(CPIW ~ m, cpiw)
  # Don't need m anymore
  cpiw$m <- NULL




# Cost of Living Adjustment
  cola <- read_ods("./COLA.ods",
                   sheet = 1,
                   col_names = TRUE)
  cola$Status <- "Actual"




# Create the full retirement age LUT (up to the current year)
  thisYear <- lubridate::year(Sys.Date())
  fra <- data.frame(`Birth Year` = c(1937:thisYear),
                    `Full Retirement Age` = c(65, 65+2/12, 65+4/12, 65+6/12, 65+8/12,
                                              65+10/12, rep(66, times = 12), 66+2/12,
                                              66+4/12, 66+6/12, 66+8/12, 66+10/12,
                                              rep(67, times = (thisYear-1960+1))),
                    check.names = FALSE)



# Save all objects in sysdata.rda
  devtools::use_data(awi, awiMars, benefitBases, benefitBasesMars,
                     piaBends, piaAdjustments,
                     cpiw, cpiwMars, cola, fra,
                     pkg = "../",
                     internal = TRUE, overwrite = TRUE)
}

setwd(paste0(rprojroot::find_rstudio_root_file(),"/data-raw"))
makeDataObjects()
setwd(rprojroot::find_rstudio_root_file())




