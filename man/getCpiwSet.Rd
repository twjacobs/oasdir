% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{getCpiwSet}
\alias{getCpiwSet}
\title{Return the \emph{Comsumer Price Wage Index} data set.}
\usage{
getCpiwSet(endYear = NULL)
}
\arguments{
\item{endYear}{the last year to include in the returned data set}
}
\value{
the CPIW data set
}
\description{
Legislation enacted in 1973 provides for cost-of-living adjustments,
or COLAs. With COLAs, Social Security and Supplemental Security Income
(SSI) benefits keep pace with inflation. The Social Security Act
specifies a formula for determining each COLA. According to the formula,
COLAs are based on increases in the
\href{https://www.ssa.gov/OACT/STATS/cpiw.html}{Consumer Price Index for Urban Wage Earners and Clerical Workers}
(CPI-W). CPI-Ws are calculated on a monthly basis by the Bureau of Labor
Statistics. This data set comes from the Social Security web site but it
is not a real time fetch, so the data is current up to the time that this
package was created. In order to determine the Consumer Price Index for a
year that is beyond the last year in the data, the data is fit with a
model which is used to project future CPIW values. If no endYear is
provided the function returns the data set of measured values without
any projected values.
}
\examples{
getCpiwSet(endYear = 2025)
getCpiwSet()
}
