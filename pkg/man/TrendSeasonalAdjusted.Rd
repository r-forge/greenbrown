\name{TrendSeasonalAdjusted}
\alias{TrendSeasonalAdjusted}
\title{Trend estimation based on seasonal-adjusted time series}
\description{The function computes and substracts the seasonal cycle from a time series. Then a trend is estimated on the seasonal-adjusted time series. The function can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}. A detailed description of this method can be found in Forkel et al. (2013).}
\usage{TrendSeasonalAdjusted(Yt, mosum.pval = 0.05, h = 0.15, breaks = NULL, 
    funSeasonalCycle = MeanSeasonalCycle)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{mosum.pval}{Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	}
  \item{h}{minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{breaks}{maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{funSeasonalCycle}{a function to estimate the seasonal cycle of the time series. A own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class "ts". Currently two approaches are part of this package:
\itemize{ 
\item{ \code{\link{MeanSeasonalCycle}} is the default which computes the average seasonal cycle from all years. }
\item{ \code{\link{SSASeasonalCycle}} can be used which detects a modulated seasonal cycle based on Singular Spectrum Analysis. }
}}
}

\value{The function returns a list of class "Trend" with the following components:
\itemize{ 
\item{ \code{series} time series on which the trend was calculated. }
\item{ \code{trend} time series with the estimated trend. }
\item{ \code{time} a vector of time steps. }
\item{ \code{bp} an object of class \code{"breakpoints"}. See \code{\link{breakpoints}} for details. }
\item{ \code{slope} a vector of the trend slopes for each trend segment. }
\item{ \code{pval} a vector of the p-values of teh trend for each trend segment. }
}}
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Trend}}, \code{\link{TrendRaster}}, \code{\link{MeanSeasonalCycle}}, \code{\link{SSASeasonalCycle}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on time series with removed mean seasonal cycle
MACtrend <- TrendSeasonalAdjusted(ndvi, funSeasonalCycle=MeanSeasonalCycle)
MACtrend
plot(MACtrend)

# plot the seasonal-adjusted time series
plot(ndvi)
lines(MACtrend$adjusted, col="orange")

# calculate trend on time series with removed mean seasonal cycle but with limited number of breakpoints
MACtrend <- TrendSeasonalAdjusted(ndvi, breaks=1, funSeasonalCycle=MeanSeasonalCycle)
plot(MACtrend)

# calculate trend on time series with removed seasonal cycle but seasonal cycle computed based on singular spectrum analysis
SSAtrend <- TrendSeasonalAdjusted(ndvi, funSeasonalCycle=SSASeasonalCycle)
SSAtrend 
plot(SSAtrend)
lines(SSAtrend$adjusted, col="orange")


}
