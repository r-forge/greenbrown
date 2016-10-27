\name{SimRem}
\alias{SimRem}
\title{Simulate the short-term variability component of a surrogate time series}
\description{The function simulates the short-term variability component (remainder component) of a time series (remainder component) based on normal-distributed random values.}
\usage{SimRem(sd = 0.05, range = sd * 3, n = 360, start = c(1982, 1), 
    freq = 12)}
\arguments{
  \item{sd}{standard deviation of short-term anomalies}
  \item{range}{range of short-term anomalies}
  \item{n}{length of the time series}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.}
}

\value{time series of class \code{\link[stats]{ts}}}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SimTs}}}
\examples{
Rt <- SimRem(sd=0.02, range=0.08)
plot(Rt)
}
