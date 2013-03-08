\name{SimIAV}
\alias{SimIAV}
\title{Simulate the inter-annual variability component of a surrogate time series}
\description{The function simulates the inter-annual variability component of a time series based on normal-distributed random values.}
\usage{SimIAV(sd = 0.015, range = sd * 2, nyears = 30, start = c(1982, 
    1), freq = 12)}
\arguments{
  \item{sd}{standard deviation of the annual mean values}
  \item{range}{range of the annual mean values}
  \item{nyears}{number of years}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.}
}

\value{time series of class \code{\link[stats]{ts}}}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{SimTs}}}
\examples{
It <- SimIAV(sd=0.015, range=0.05, nyears=30)
plot(It)
}
