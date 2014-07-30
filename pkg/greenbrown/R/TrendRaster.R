TrendRaster <- structure(function(
	##title<< 
	## Calculate trends on time series in gridded (raster) data
	##description<<
	## This function computes temporal trend and trend breakpoints on multi-temporal raster data. To calculate trends on the values of each grid cell the function \code{\link{Trend}} is used. Before using these methods on satellite time series (especially NDVI time series) the descriptions and recommendations in Forkel et al. (2013) should be considered.

	r, 
	### multi-layer raster object of class \code{\link[raster]{brick}}
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12,
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.
	
	method=c("AAT", "STM", "SeasonalAdjusted"), 
	### method to be used for trend calculation with the following options: 
	### \itemize{
	### \item{ \code{AAT} (default) calculates trends on annual aggregated time series (see \code{\link{TrendAAT}} for details). This method will be automatically choosen if the time series has a frequency of 1 (e.g. in case of annual time steps). If the time series has a frequency > 1, the time series will be aggregated to annual time steps using the mean. }
	### \item{ \code{STM} fits harmonics to the seasonal time series to model the seasonal cycle and to calculate trends based on a multiple linear regression (see \code{\link{TrendSTM}} for details). }
	### \item{ \code{SeasonalAdjusted} removes first the seasonal cycle from the time series and calculates the trend on the reaminder series (see \code{\link{TrendSeasonalAdjusted}} for details). }
	### }
	
	mosum.pval=0.05,
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	
	
	h=0.15, 
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.
	
	breaks=0, 
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.
	
	funSeasonalCycle=MeanSeasonalCycle,
	### a function to estimate the seasonal cycle of the time series if \code{SeasonalAdjusted} is selected as method. An own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class \code{\link{ts}}. Currently two approaches are part of this package:
	### \itemize{ 
	### \item{ \code{\link{MeanSeasonalCycle}} is the default which computes the mean seasonal cycle. }
	### \item{ \code{\link{SSASeasonalCycle}} detects a modulated seasonal cycle based on Singular Spectrum Analysis. }
	### }
	
	funAnnual=mean,
	### function to aggregate time series to annual values if \code{AAT} is selected as method. The default function is the mean (i.e. trend calculated on mean annual time series). See \code{TrendAAT} for other examples
	
	...
	### additional arguments as for \code{\link{writeRaster}}
	
	##details<<
	## The maximum number of breakpoints should be specified in this function. If \code{breaks=0} no breakpoints will be computed. If \code{breaks=1} one breakpoint can be detected at maximum per grid cell. In this case the result will be reported for two time series segments (SEG1 before the breakpoint, SEG2 after the breakpoint). 
	## Some of the trend methods are very slow. Applying them on multi-temporal raster datasets can take some time. Especially the methods that work on the full temporal resolution time series (STM and SeasonalAdjusted) are slower than the method AAT. Especially if breakpoints are computed the computations take longer. The computation of breakpoints can be suppressed by choosing breaks=0. For large rasters it is recommended to first split the raster dataset in several tiles and to compute the trends on each tile separately. The use of a high performance computing infrastructure it also advantageous. 
	## All methods work with missing observations (for example missing NDVI observation in winter months with snow cover). Missing observation have to be flagged with NA. All time steps have to be included in the RasterBrick for trend analysis. If complete time steps are missing, they need to be included as layers (filled with NA values) in the RasterBrick to form a continuous time series. 

	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.

	##seealso<<
	## \code{\link{Trend}}, \code{\link{TrendClassification}}, \code{\link{TrendSegmentsRaster}}, \code{\link{NamesTrendRaster}} 
) {

	# select method for trend calculation
	if (freq == 1) method <- "AAT"
	if (is.null(breaks)) {
		breaks <- 8
		warning("breaks == NULL is not used in function TrendRaster. breaks (maximal number of breaks to be calculated was changed to 8.") 
	}
	
	# define function to apply on RasterBrick
	.funForRaster <- function(x) {
		# convert to ts
		x <- ts(as.vector(x), start=start, freq=freq)
		
		# length of result vector
		if (breaks > 0) length.res <- length(c(1:(breaks+1), 1:breaks, 1:(breaks+1), 1:(breaks+1)))
		if (breaks == 0) length.res <- length(c(1:(breaks+1), 1:(breaks+1), 1:(breaks+1)))
		# x.global <<- x
		
		# return NA if less than 3 values are NA
		if ((sum(is.na(x))/length(x) > 0.7) | (AllEqual(x))) return(rep(NA, length.res))

		# apply the function for trend analysis 
		result.l <- do.call(Trend, list(x, breaks=breaks, h=h, method=method, funSeasonalCycle=funSeasonalCycle, mosum.pval=mosum.pval, funAnnual=funAnnual, sample.method="none"))
		
		# get breakpoints and length of segments
		bp <- result.l$bp$breakpoints	# get breakpoints
		bp.dates.res <- rep(NA, breaks)	# resulting vector for breakdates
		seg.length.res <- rep(NA, breaks+1)	# resulting vector for segment length
		if (!is.na(bp[1])) {
			bp.dates <- as.numeric(result.l$time[result.l$bp$breakpoints])	# get breakdate
			bp.dates.res[1:length(bp.dates)] <- bp.dates					# copy breakdate to result vector
			limits <- c(result.l$bp$breakpoints, length(result.l$series))	# get ends of segments
			seg.length <- c(limits[1], limits[2:length(limits)] - limits[1:(length(limits)-1)])	
			seg.length.res[1:length(seg.length)] <- seg.length
		} else {
			seg.length.res[1] <- length(result.l$series)
		}
		
		# get slopes
		slopes <- result.l$slope
		slopes.res <- rep(NA, breaks+1)
		slopes.res[1:length(slopes)] <- slopes
		
		# get p values
		pvals <- result.l$pval
		pvals.res <- rep(NA, breaks+1)
		pvals.res[1:length(pvals)] <- pvals
			
		# return result as vector
		result <- c(seg.length.res, bp.dates.res, slopes.res, pvals.res)
		return(result)
	}
	
	# apply function on raster
	r.trend <- calc(r, fun=.funForRaster, ...)	
	
	# names for raster layers
	names(r.trend) <- NamesTrendRaster(breaks)
	return(r.trend)
	### The function returns a RasterBrick with different trend and breakpoint statistics. The layers are named:
	### \itemize{ 
	### \item{ \code{LengthSEG} length of the time series segment }
	### \item{ \code{BP} date of the trend breakpoints }
	### \item{ \code{SlopeSEG} slope of the trend in each segment }
	### \item{ \code{PvalSEG} p-value of the trend in each segment }
	### }
	### The choosen number of \code{breaks} will define the number of raster layers of the result.  
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
ndvimap
plot(ndvimap, 8)

# attention: the following examples can take some time!

# calculate trend: annual aggregation method
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=2)
plot(AATmap)

# calculate trend: seasonal-trend model
STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=2)
plot(STMmap)

# compare trend estimates
brks.slope <- seq(-0.004, 0.004, by=0.001) # define class breaks and colors for the results
cols.slope <- brgr.colors(length(brks.slope)-1)
brks.bp <- seq(1980, 2010, 5)
cols.bp <- brgr.colors(length(brks.bp)-1)
par(mfrow=c(2,3)) # set the tiles of the plot
plot(AATmap, 4, col=cols.bp, breaks=brks.bp)
plot(AATmap, 6, col=cols.slope, breaks=brks.slope)
plot(AATmap, 7, col=cols.slope, breaks=brks.slope)
plot(STMmap, 4, col=cols.bp, breaks=brks.bp)
plot(STMmap, 6, col=cols.slope, breaks=brks.slope)
plot(STMmap, 7, col=cols.slope, breaks=brks.slope)
par(mfrow=c(1,1))

# calculate trend: seasonal adjusted time series based on mean annual cycle, no breakpoints
MACmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="SeasonalAdjusted", breaks=0, funSeasonalCycle=MeanSeasonalCycle)
plot(MACmap)

# calculate trend: seasonal adjusted time series based on singular spectrum analysis, no breakpoints
SSAmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="SeasonalAdjusted", breaks=0, funSeasonalCycle=SSASeasonalCycle)
plot(SSAmap)

# classify the results in greening/browning/no trend
MACmap.cl <- TrendClassification(MACmap, min.length=(8*12))
SSAmap.cl <- TrendClassification(SSAmap, min.length=(8*12))
par(mfrow=c(1,2)) # set the tiles of the plot
plot(MACmap.cl, col=brgr.colors(3))
plot(SSAmap.cl, col=brgr.colors(3))

citation("greenbrown")
})

