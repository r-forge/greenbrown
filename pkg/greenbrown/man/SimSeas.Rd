\name{SimSeas}
\alias{SimSeas}
\title{Simulate the seasonal component of a surrogate time series}
\description{The function simulates the seasonal component of a time series based on a cosinus harmonic term.}
\usage{SimSeas(range, n = 360, start = c(1982, 1), freq = 12)}
\arguments{
  \item{range}{range of the seasonal cycle (seasonal amplitude)}
  \item{n}{length of the time series}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.}
}

\value{time series of class \code{\link[stats]{ts}}}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{SimTs}}}
\examples{
St <- SimSeas(range=0.6)
plot(St)
}
