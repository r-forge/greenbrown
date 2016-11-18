\name{SimTrend}
\alias{SimTrend}
\title{Simulate trend and breakpoints of a surrogate time series}
\description{The function simulates the trend component with breakpoints of a time series.}
\usage{SimTrend(slope = c(0.002, -0.004), breaks = 165, abrupt = TRUE, 
    n = 360, start = c(1982, 1), freq = 12)}
\arguments{
  \item{slope}{slope of the trend in each time series segment. \code{slope} should be a numeric vector. The length of this vector determines the number of segments.}
  \item{breaks}{position of the breakpoints in the time series. You should specify one more slope than breakpoint.}
  \item{abrupt}{Should the trend at the breakpoints change abrupt (\code{TRUE}) or gradual (\code{FALSE})?}
  \item{n}{length of the time series}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.}
}

\value{time series of class \code{\link[stats]{ts}}}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SimTs}}}
\examples{
Tt <- SimTrend(slope=c(0.003, -0.001), breaks=150)
plot(Tt)
}
