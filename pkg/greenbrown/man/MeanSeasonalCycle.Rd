\name{MeanSeasonalCycle}
\alias{MeanSeasonalCycle}
\title{Calculate the mean seasonal cycle of a time series}
\description{The function calculates the mean seasonal cycle of a time series. }
\usage{MeanSeasonalCycle(ts)}
\arguments{
  \item{ts}{univariate time series of class \code{\link{ts}}}
}

\value{Mean seasonal cycle of time series \code{ts} with the same length as \code{ts}, i.e. the mean seasonal cycle is repeated for each year. The mean seasonal cycle is centered to 0.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Decompose}}, \code{\link{TrendSeasonalAdjusted}}}
\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)
ndvi.cycle <- MeanSeasonalCycle(ndvi)
plot(ndvi.cycle)

# the mean seasonal cycle is centered to 0, 
# add the mean of the time series if you want to overlay it with the original data:
plot(ndvi)
lines(ndvi.cycle + mean(ndvi, na.rm=TRUE), col="blue")
}
