\name{Trend}
\alias{Trend}
\title{Calculate trends and trend changes in time series}
\description{This function calculates trends and trend changes (breakpoints) in a time series. It is a common interface to the functions \code{\link{TrendAAT}}, \code{\link{TrendSTM}} and \code{\link{TrendSeasonalAdjusted}}. With \code{\link{TrendRaster}} all trend analysis functions can be applied to gridded (raster) data. A detailed description of these methods can be found in Forkel et al. (2013).}
\usage{Trend(Yt, method = c("AAT", 
    "STM", "SeasonalAdjusted", 
    "RQ"), mosum.pval = 0.05, 
    h = 0.15, breaks = NULL, 
    funSeasonalCycle = MeanSeasonalCycle, 
    funAnnual = mean)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{method}{\code{method} to be used for trend calculation with the following options: 
\itemize{
\item{ \code{AAT} (default) calculates trends on annual aggregated time series (see \code{\link{TrendAAT}} for details). This \code{method} will be automatically choosen if the time series has a frequency of 1 (e.g. in case of annual time steps). If the time series has a frequency > 1, the time series will be aggregated to annual time steps using the mean. }
\item{ \code{STM} fits harmonics to the seasonal time series to model the seasonal cycle and to calculate trends based on a multiple linear regression (see \code{\link{TrendSTM}} for details). }
\item{ \code{SeasonalAdjusted} removes first the seasonal cycle from the time series and calculates the trend on the reaminder series (see \code{\link{TrendSeasonalAdjusted}} for details). }
\item{ \code{RQ} computes trends based on quantile regression (see \code{\link{TrendRQ}} for details). }
}}
  \item{mosum.pval}{Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	}
  \item{h}{minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{breaks}{maximal number of \code{breaks} to be calculated (integer number). By default the maximal number allowed by \code{h} is used. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{funSeasonalCycle}{a function to estimate the seasonal cycle of the time series if \code{SeasonalAdjusted} is selected as \code{method}. A own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class \code{\link{ts}}. Currently two approaches are part of this package:
\itemize{ 
\item{ \code{\link{MeanSeasonalCycle}} is the default which computes the mean seasonal cycle. }
\item{ \code{\link{LmSeasonalCycle}} detects a seasonal cycle based on a linear regression with harmonic terms. }
}}
  \item{funAnnual}{function to aggregate time series to annual values if \code{AAT} is selected as \code{method}. The default function is the mean (i.e. trend calculated on mean annual time series). See \code{\link{TrendAAT}} for other examples}
}
\details{This function allows to calculate trends and trend changes based on different methods: see \code{\link{TrendAAT}}, \code{\link{TrendSTM}} or \code{\link{TrendSeasonalAdjusted}} for more details on these methods.
These methods can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}.}
\value{The function returns a list of class "Trend". }
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{plot.Trend}}, \code{\link{TrendAAT}}, \code{\link{TrendSTM}}, \code{\link{TrendSeasonalAdjusted}}, \code{\link{TrendRaster}}, \code{\link{breakpoints}}}
\examples{
# calculate trend (default method: trend calculated based on annual aggregated data)
trd <- Trend(ndvi)
trd
plot(trd)

# an important parameter is mosum.pval: if the p-value is changed to 1, 
# breakpoints can be detected in the time series regardless if 
# there is significant structural change
trd <- Trend(ndvi, mosum.pval=1)
trd
plot(trd)

# calculate trend based on modelling the seasonal cycle
trd <- Trend(ndvi, method="STM")
trd
plot(trd)

# calculate trend based on removal of the seasonal cycle
trd <- Trend(ndvi, method="SeasonalAdjusted", funSeasonalCycle=MeanSeasonalCycle)
plot(trd)
lines(trd$adjusted, col="green")
trd

# modify maximal number of breakpoints
trd <- Trend(ndvi, method="SeasonalAdjusted", breaks=1)
plot(trd)
trd

# use quantile regression
trd <- Trend(ndvi, method="RQ")
plot(trd)
trd


}
