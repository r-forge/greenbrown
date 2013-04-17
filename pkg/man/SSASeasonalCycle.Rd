\name{SSASeasonalCycle}
\alias{SSASeasonalCycle}
\title{Calculate a modulated seasonal cycle of a time series based on singular spectrum analysis (SSA)}
\description{The function calculates a seasonal cycle based on 1-D singular spectrum analysis (Golyandina et al. 2001) as implemented in the Rssa package. See \code{\link[Rssa]{ssa}} for details. }
\usage{SSASeasonalCycle(ts)}
\arguments{
  \item{ts}{univariate time series of class \code{\link[stats]{ts}}}
}


\references{Golyandina, N., Nekrutkin, V. and Zhigljavsky, A. (2001): Analysis of Time Series Structure: SSA and related techniques. Chapman and Hall/CRC. ISBN 1584881941 }
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TrendSeasonalAdjusted}}, \code{\link[Rssa]{new.ssa}}, \code{\link[Rssa]{reconstruct}}}
\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# estimate the seasonal cycle using SSA
ndvi.cycle <- SSASeasonalCycle(ndvi)
plot(ndvi.cycle)

# the mean seasonal cycle is centered to 0 - add the mean of the time series if you want to overlay it with the original data:
plot(ndvi)
lines(ndvi.cycle + mean(ndvi, na.rm=TRUE), col="blue")
	
}
