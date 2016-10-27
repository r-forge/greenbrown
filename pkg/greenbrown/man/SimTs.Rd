\name{SimTs}
\alias{SimTs}
\title{Simulate surrogate time series}
\description{The function simulates a surrogate (artificial) time series based on the defined properties. See Forkel et al. 2013 for a description how time series are simulated with this function.}
\usage{SimTs(M = 0.35, Tslope = c(0.002, -0.004), Isd = 0.015, Irange = 0.03, 
    Srange = 0.5, Rsd = 0.05, Rrange = 0.1, breaks = 120, abrupt = TRUE, 
    n = 360, start = c(1982, 1), freq = 12)}
\arguments{
  \item{M}{mean of the time series}
  \item{Tslope}{slope of the trend in each time series segment. \code{slope} should be a numeric vector. The length of this vector determines the number of segements.}
  \item{Isd}{standard deviation of the annual mean values (inter-annual variability)}
  \item{Irange}{range of the annual mean values (inter-annual variability)}
  \item{Srange}{range of the seasonal cycle (seasonal amplitude)}
  \item{Rsd}{standard deviation of short-term anomalies}
  \item{Rrange}{range of short-term anomalies}
  \item{breaks}{position of the breakpoints in the time series. You should specify one more slope than breakpoint.}
  \item{abrupt}{Should the trend at the breakpoints change abrupt (\code{TRUE}) or gradual (\code{FALSE})?	}
  \item{n}{length of the time series}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.}
}

\value{The function returns multiple time series of class \code{\link[stats]{ts}} with the following components: total time series, mean, trend component, inter-annual variability component, seasonal component, short-term component.}
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SimTs}}}
\examples{
# simulate artificial time series
x <- SimTs(M=0.4, Tslope=0.0008, Isd=0.015, Irange=0.03, Srange=0.5, Rsd=0.05, 
	Rrange=0.1, breaks=NULL, abrupt=TRUE, n=360, start=c(1982, 1), freq=12)
plot(x)

x <- SimTs(M=0.35, Tslope=c(0.002, -0.0015), Isd=0.015, Irange=0.03, Srange=0.5, Rsd=0.05, 
	Rrange=0.1, breaks=120, abrupt=TRUE, n=360, start=c(1982, 1), freq=12)
plot(x)

x <- SimTs(M=0.4, Tslope=c(0.003, -0.001, 0), Isd=0.03, Irange=0.08, Srange=0.3, Rsd=0.06, 
	Rrange=0.2, breaks=c(100, 210), abrupt=FALSE, n=360, start=c(1982, 1), freq=12)
plot(x)

}
