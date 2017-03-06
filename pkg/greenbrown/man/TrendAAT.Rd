\name{TrendAAT}
\alias{TrendAAT}
\title{Trend estimation based on annual aggregated time series}
\description{The function aggregates a time series to annual values and computes breakpoints and trends on the annual aggregated time series. The function can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}. A detailed description of this method can be found in Forkel et al. (2013).}
\usage{TrendAAT(Yt, mosum.pval = 0.05, h = 0.15, breaks = NULL, funAnnual = mean)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{mosum.pval}{Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	}
  \item{h}{minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{breaks}{maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{funAnnual}{function to aggregate time series to annual values The default function is the mean (i.e. trend calculated on mean annual time series). See example section for other examples.}
}

\value{The function returns a list of class "Trend". }
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Trend}}, \code{\link{TrendRaster}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd.annualmean <- TrendAAT(ndvi)
trd.annualmean
plot(trd.annualmean)

TrendLongestSEG(trd.annualmean)

# calculate annual trend but don't apply MOSUM test for structural change
trd.annualmean <- TrendAAT(ndvi, mosum.pval=1)
trd.annualmean
plot(trd.annualmean)

# calculate trend on annual peak (maximum) NDVI
trd.annualmax <- TrendAAT(ndvi, funAnnual=max, mosum.pval=1)
trd.annualmax 
plot(trd.annualmax)

# calculate trend on an annual quantile NDVI (e.g. upper 0.9 quantile)
fun <- function(x, ...) { quantile(x, 0.9, ...) }
trd.annualquantile9 <- TrendAAT(ndvi, funAnnual=fun, mosum.pval=1)
trd.annualquantile9
plot(trd.annualquantile9)

# calculate trend on an winter NDVI (e.g. upper 0.1 quantile)
fun <- function(x, ...) { quantile(x, 0.1, ...) }
trd.annualquantile1 <- TrendAAT(ndvi, funAnnual=fun, mosum.pval=1)
trd.annualquantile1
plot(trd.annualquantile1)

# compare trends
plot(ndvi)
plot(trd.annualmean, add=TRUE, col="darkgreen", symbolic=TRUE)
plot(trd.annualmax, add=TRUE, col="red", symbolic=TRUE)
plot(trd.annualquantile9, add=TRUE, col="orange", symbolic=TRUE)
plot(trd.annualquantile1, add=TRUE, col="blue", symbolic=TRUE)
}
