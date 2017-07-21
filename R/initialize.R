
# load:
# -Average Wage Index Series
# -Cola History
# -Contribution and Benefits Bases
# -Consumer Price Index for Urban Wages and Clerical Workers
# -Primary Insurance Amount Adjustments
# -Primary Insurance Amount Bends

load("R/sysdata.rda")


thisYear <- lubridate::year(Sys.Date())

