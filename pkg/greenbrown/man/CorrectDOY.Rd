\name{CorrectDOY}
\alias{CorrectDOY}
\title{Correct day-of-year time series}
\description{This function corrects a time series with days-of-years (e.g. start of growing season). For example, if the start of season occurs in one year at the end of the calendar year (doy > 305) and in another year at the beginning (doy < 60), the DOYs are corrected so that all values occur at the beginning of the year (e.g. negative DOYs will be produced) or at the end of the year (e.g. DOY > 365 will be produced). This function is applied in \code{\link{Phenology}} after phenology detection on sos, eos, pop and pot time series (see examples).}
\usage{CorrectDOY(doy, check.outliers = TRUE)}
\arguments{
  \item{doy}{a vector or time series representing DOYs}
  \item{check.outliers}{Set outliers to NA after correction? Outliers are defined here as: doy < (median - IQR*2) | doy > (median + IQR*2))}
}

\value{a vector or time series}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Phenology}}}
\examples{

# imagine the following start of season DOYs in 10 years
sos <- ts(c(15, 10, 12, 8, 10, 3, 362, 2, 1, 365), start=1982) # most values occur at the beginning of the year
plot(sos)
# Visually, there seems to be big differences in the start of season. However, there is actually 
# only one day difference between the last two values (DOY 1 = 1st January, DOY 365 = 31st December)! 
# Trend calculation fails on this time series:
plot(Trend(sos), ylab="SOS") 

# The DOY time series needs to be corrected to analyze the true differences between days.
sos2 <- CorrectDOY(sos)
plot(Trend(sos2), ylab="SOS") 
# The correction now allows trend analysis.
# Negative DOYs indicate days at the end of the previous year!

# other example
sos <- ts(c(5, 12, 15, 120, 363, 3, 362, 365, 360, 358), start=1982) # most values occur at the end of the year
plot(sos) # one value seems like an outlier
sos2 <- CorrectDOY(sos)
plot(Trend(sos2), ylab="SOS") 
# The outlier is removed.
# DOYs > 365 indicate days in the next year!

}
