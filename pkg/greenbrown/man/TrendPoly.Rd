\name{TrendPoly}
\alias{TrendPoly}
\title{Trend estimation based on a 4th order polynomial}
\description{The function computes a trend based on a 4th order polynomial function.}
\usage{TrendPoly(Yt, ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{\dots}{additional arguments (currently not used)}
}

\value{The function returns a list of class "Trend". }

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{stl}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd <- TrendPoly(ndvi)
trd
plot(trd)

}
