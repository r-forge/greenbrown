\name{TrendSpline}
\alias{TrendSpline}
\title{Trend estimation based on a smoothing splines}
\description{The function computes a non-linear trend based on \code{\link{smooth.spline}}. }
\usage{TrendSpline(Yt, ...)}\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{\dots}{additional arguments (currently not used)}
}

\value{The function returns a list of class "Trend". }

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{stl}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd <- TrendSpline(ndvi)
trd
plot(trd)

}
