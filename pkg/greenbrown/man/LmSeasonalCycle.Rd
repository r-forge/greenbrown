\name{LmSeasonalCycle}
\alias{LmSeasonalCycle}
\title{Calculate the mean seasonal cycle of a time series based on a linear model}
\description{The function calculates the mean seasonal cycle of a time series based on a linear regression between the values and the time. Therefore a linear model with interactions is fitted to the original values Y of the form: Y = (a * m) * (b * sin(m)) * (c * cos(m)) + d where m are the the seasonal indices (e.g. months).  }
\usage{LmSeasonalCycle(ts)}
\arguments{
  \item{ts}{univariate time series of class \code{\link{ts}}}
}

\value{Mean seasonal cycle of time series \code{ts} with the same length as \code{ts}, i.e. the mean seasonal cycle is repeated for each year. The mean seasonal cycle is centered to 0.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Decompose}}, \code{\link{TrendSeasonalAdjusted}}, \code{\link{MeanSeasonalCycle}}}
\examples{
ndvi.lmcycl <- LmSeasonalCycle(ndvi)
plot(ndvi.lmcycl)

ndvi.meancycl <- MeanSeasonalCycle(ndvi)
plot(ndvi.lmcycl[1:12], col="red", type="l")
lines(ndvi.meancycl[1:12], col="blue")

}
