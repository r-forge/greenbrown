\name{TrendRaster}
\alias{TrendRaster}
\title{Calculate trends on time series in gridded (raster) data}
\description{This function computes temporal trend and trend breakpoints on multi-temporal raster data. To calculate trends on the values of each grid cell the function \code{\link{Trend}} is used. Before using these methods on satellite time series (especially NDVI time series) the descriptions and recommendations in Forkel et al. (2013) should be considered.}
\usage{TrendRaster(r, start = c(1982, 
    1), freq = 12, method = c("AAT", 
    "STM", "SeasonalAdjusted"), 
    mosum.pval = 0.05, 
    h = 0.15, breaks = 0, 
    funSeasonalCycle = MeanSeasonalCycle, 
    funAnnual = mean, 
    ...)}
\arguments{
  \item{r}{multi-layer raster object of class \code{\link[raster]{brick}}}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual \code{start} date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.}
  \item{method}{\code{method} to be used for trend calculation with the following options: 
\itemize{
\item{ \code{AAT} (default) calculates trends on annual aggregated time series (see \code{\link{TrendAAT}} for details). This \code{method} will be automatically choosen if the time series has a frequency of 1 (e.g. in case of annual time steps). If the time series has a frequency > 1, the time series will be aggregated to annual time steps using the mean. }
\item{ \code{STM} fits harmonics to the seasonal time series to model the seasonal cycle and to calculate trends based on a multiple linear regression (see \code{\link{TrendSTM}} for details). }
\item{ \code{SeasonalAdjusted} removes first the seasonal cycle from the time series and calculates the trend on the reaminder series (see \code{\link{TrendSeasonalAdjusted}} for details). }
}}
  \item{mosum.pval}{Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	}
  \item{h}{minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{breaks}{maximal number of \code{breaks} to be calculated (integer number). By default the maximal number allowed by \code{h} is used. See \code{\link[strucchange]{breakpoints}} for details.}
  \item{funSeasonalCycle}{a function to estimate the seasonal cycle of the time series if \code{SeasonalAdjusted} is selected as \code{method}. An own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class \code{\link{ts}}. Currently two approaches are part of this package:
\itemize{ 
\item{ \code{\link{MeanSeasonalCycle}} is the default which computes the mean seasonal cycle. }
\item{ \code{\link{LmSeasonalCycle}} detects a seasonal cycle based on a linear regression with harmonic terms. }
}}
  \item{funAnnual}{function to aggregate time series to annual values if \code{AAT} is selected as \code{method}. The default function is the mean (i.e. trend calculated on mean annual time series). See \code{TrendAAT} for other examples}
  \item{\dots}{additional arguments as for \code{\link{writeRaster}}}
}
\details{The maximum number of breakpoints should be specified in this function. If \code{breaks=0} no breakpoints will be computed. If \code{breaks=1} one breakpoint can be detected at maximum per grid cell. In this case the result will be reported for two time series segments (SEG1 before the breakpoint, SEG2 after the breakpoint). 
Some of the trend methods are very slow. Applying them on multi-temporal raster datasets can take some time. Especially the methods that work on the full temporal resolution time series (STM and SeasonalAdjusted) are slower than the \code{method} AAT. Especially if breakpoints are computed the computations take longer. The computation of breakpoints can be suppressed by choosing breaks=0. For large rasters it is recommended to first split the raster dataset in several tiles and to compute the trends on each tile separately. The use of a high performance computing infrastructure it also advantageous. 
All methods work with missing observations (for example missing NDVI observation in winter months with snow cover). Missing observation have to be flagged with NA. All time steps have to be included in the RasterBrick for trend analysis. If complete time steps are missing, they need to be included as layers (filled with NA values) in the RasterBrick to form a continuous time series. }
\value{The function returns a RasterBrick with different trend and breakpoint statistics. The layers are named:
\itemize{ 
\item{ \code{LengthSEG} length of the time series segment }
\item{ \code{BP} date of the trend breakpoints }
\item{ \code{SlopeSEG} slope of the trend in each segment }
\item{ \code{PvalSEG} p-value of the trend in each segment }
}
The choosen number of \code{breaks} will define the number of raster layers of the result.  }
\references{Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Trend}}, \code{\link{TrendClassification}}, \code{\link{TrendSegmentsRaster}}, \code{\link{NamesTrendRaster}} }
\examples{
# 
# # calculate trend: annual aggregation method
# AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=1)
# plot(AATmap)
# 
# # trend on seasonal adjusted time series based, no breakpoints
# MACmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, 
#    method="SeasonalAdjusted", breaks=0, funSeasonalCycle=MeanSeasonalCycle)
# plot(MACmap)
# 
# # trend based on season-trend model 
# STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=0)
# plot(STMmap)
# 
# # classify the results in greening/browning/no trend
# MACmap.cl <- TrendClassification(MACmap, min.length=(8*12))
# STMmap.cl <- TrendClassification(STMmap, min.length=(8*12))
# par(mfrow=c(1,2)) # set the tiles of the plot
# plot(MACmap.cl, col=brgr.colors(3), main="Method MAC")
# plot(STMmap.cl, col=brgr.colors(3), main="Method STM")

}
