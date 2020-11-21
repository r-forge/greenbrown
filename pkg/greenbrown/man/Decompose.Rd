\name{Decompose}
\alias{Decompose}
\title{Simple decomposition of time series}
\description{This function decomposes time series in different components using a simple step-wise approach.}
\usage{Decompose(Yt, breaks = 0, 
    mosum.pval = 0.05)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{breaks}{maximal number of \code{breaks} in the trend component to be calculated (integer number). }
  \item{mosum.pval}{Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not.}
}
\details{The decomposition of the time series is based on a simple step-wise approach:
\itemize{
\item{ The mean of the NDVI time series is calculated. }
\item{ In the second step, monthly values are aggregated per year by using the average value and the trend is calculated based on annual aggregated values using \code{ \link{TrendAAT}}. }
\item{ The mean of the time series and the derived trend component from step (2) are subtracted from the annual values to derive the trend-removed and mean-centred annual values (annual anomalies). If the trend slope is not significant (p > 0.05), only the mean is subtracted. }
\item{ In the next step, the mean, the trend component and the annual anomalies are subtracted from the original time series to calculate a detrended, mean-centered and for annual anomalies adjusted time series. From this time series the seasonal cycle is estimated as the mean seasonal cycle. }
\item{ In the last step, the short-term anomalies are computed. For this, the mean, the trend component, the annual anomalies and the mean seasonal cycle are subtracted from the original time series. } }}
\value{The function returns a multi-variate object of class ts including the time series components.}
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{GetTsStatisticsRaster}} 	}
\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# decompose this time series
ndvi.dc <- Decompose(ndvi)
plot(ndvi.dc)
	
ndvi.dc2 <- Decompose(ndvi, breaks=2, mosum.pval=1)
plot(ndvi.dc2)

}
