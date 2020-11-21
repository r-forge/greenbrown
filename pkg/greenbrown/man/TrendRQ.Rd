\name{TrendRQ}
\alias{TrendRQ}
\title{Trend estimation based on quantile regression}
\description{The function computes breakpoints and trends based on quantile regression.}
\usage{TrendRQ(Yt, tau = 0.5, 
    ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{tau}{quantile to be estimated in quantile regression}
  \item{\dots}{additional arguments (currently not used)}
}

\value{The function returns a list of class "Trend". }

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Trend}}, \code{\link{TrendAAT}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
ndvi <- window(aggregate(ndvi, FUN=mean), end=1996)
plot(ndvi)
	
# calculate trend based on regression to median
trd.q05 <- TrendRQ(ndvi)
trd.q05
plot(trd.q05)

TrendLongestSEG(trd.q05)


}
