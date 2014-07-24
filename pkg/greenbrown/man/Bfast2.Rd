\name{Bfast2}
\alias{Bfast2}
\title{Break Detection in the Seasonal and Trend Component of a Univariate Time Series}
\description{The Bfast2 function is an alternative implementation of the bfast (breaks for additive seasonal and trend) algorithm. Bfast detects iteratively breaks  in seasonal and trend component of a time series. Bfast combines the iterative decomposition of time series into trend, seasonal and remainder components with significant break detection in the decomposed components of the time series. 
The Bfast2 function is based on the original implementation of \code{\link{bfast}} as in the bfast package. In difference to the original bfast implementation, Bfast2 allows missing values (NA) in the time series and thus does not require gap filling before the application of bfast.}
\usage{Bfast2(Yt, h = 0.15, max.iter = 2, breaks = 1, hpc = "none")}
\arguments{
  \item{Yt}{univariate time series to be analyzed. This should be an object of class "ts" with a frequency greater than one. }
  \item{h}{minimal segment size between potentially detected breaks in the trend model given as fraction relative to the sample size (i.e. the minimal number of observations in each segment divided by the total length of the timeseries.}
  \item{max.iter}{maximum amount of iterations allowed for estimation of breakpoints in seasonal and trend component. }
  \item{breaks}{integer specifying the maximal number of breaks to be calculated. By default the maximal number allowed by h is used. }
  \item{hpc}{A character specifying the high performance computing support. Default is "none", can be set to "foreach". Install the "foreach" package for hpc support. }
}

\value{The function returns an object of the class "bfast", see \code{\link{bfast}} for details.}
\references{Verbesselt, J.; Hyndman, R.; Zeileis, A.; Culvenor, D., Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment 2010, 114, 2970-2980. \cr}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{bfast}}, \code{\link{plot.bfast}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# set some missing values in the data
ndvi[c(60:80, 245:246, 255)] <- NA
ndvi[ndvi < 0.22] <- NA
plot(ndvi)

# the original bfast algorithm does not work with gaps:
# bf <- bfast(ndvi, season="harmonic")

# the Bfast2 implementation can handle gaps:
bf <- Bfast2(ndvi, breaks=1)
plot(bf, type="all")
	
}
