GetTsStatisticsRaster <- structure(function(
	##title<< 
	## Estimate statistical properties of time series in a multi-temporal raster dataset
	
	##description<<
	## This function computes statistical properties of the time series in a multi-temporal raster dataset. It calls \code{\link{Decompose}} to decompose the time series of each grid cell of a raster brick into a trend, inter-annual variability, seasonal and short-term variability time series components. In a next step the mean, the trend slope, the range  and standard deviation of the inter-annual variability, the range of the seasonal cycle as well as the range and standard devaition of the short-term variability are calculated. 
	
	r, 
	### object of class \code{\link[raster]{brick}} with multi-temporal data.
	
	start=c(1982, 1), 
	### first time step, e.g. c(1982, 1) for January 1982. See \code{\link{ts}} for details.
	
	freq=12
	### the number of observations per unit of time, e.g. 12 for monthly data or 24 for bi-monthly data. See \code{\link{ts}} for details.
		
	##seealso<<
	## \code{\link{Decompose}}
	
	) {
	
	GetTsStatistics <- function(ts) {
		result <- rep(NA, 7)
		if (!AllEqual(ts)) {
			if (class(ts) != "ts") {
				ts <- ts(ts, start=start, freq=freq)
			}
			
			# decompose time series
			dc <- Decompose(ts, breaks=0)	# don't consider breakpoints in trend

			# get statistics
			dc.mean <- mean(dc[,1])	# mean of time series
			trd <- aggregate(dc[,2], FUN="mean")	
			coef <- coef(lm(trd ~ time(trd)))
			dc.trend.slope <- coef[2]	# slope of trend
			dc.iav.range <- max(dc[,3]) - min(dc[,3])	# range of IAV
			dc.iav.sd <- sd(dc[,3])	# standard deviation of IAV
			dc.seas.range <- max(dc[,4]) - min(dc[,4])	# range of seasonal cycle
			dc.rem.range <- max(dc[,5]) - min(dc[,5])	# range of remainder component
			dc.rem.sd <- sd(dc[,5]) 	# standard deviation of remainder component
			result <- c(mean=dc.mean, trend.slope=dc.trend.slope, iav.range=dc.iav.range, iav.sd=dc.iav.sd, seas.range=dc.seas.range, rem.range=dc.rem.range, rem.sd=dc.rem.sd)
			
		}
		return(result)
	} # end GetTsStatistics
	
	result <- calc(r, GetTsStatistics)
	layerNames(result) <- c("Mean", "Trend.slope", "IAV.range", "IAV.sd", 
		"Seas.range", "STV.range", "STV.sd")
	return(result)
	### The function returns a RasterBrick with 7 layers (mean, trend slope, range of IAV, standard deviation of inter-annual variabililty, range of seasonal cycle, range and standard deviaition of short-term variabililty. 
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate time series statistics 
ndvimap.tsstat <- GetTsStatisticsRaster(ndvimap)
plot(ndvimap.tsstat)
})
	