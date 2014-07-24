MeanSeasonalCycle <- structure(function(
	##title<< 
	## Calculate the mean seasonal cycle of a time series
	
	##description<<
	## The function calculates the mean seasonal cycle of a time series. 
	
	ts
	### univariate time series of class \code{\link{ts}}
		
	##seealso<<
	## \code{\link{Decompose}}, \code{\link{TrendSeasonalAdjusted}}
) {

	if (class(ts) != "ts") stop("ts should be of class ts.")
	time <- time(ts)
	years <- as.integer(time)
	nyears <- length(unique(years))
	
	# estimate sesonal cycle based on mean seasonal cycle
	St_est <- aggregate(as.vector(ts), list(as.vector(cycle(ts))), "mean", na.rm=TRUE)$x
	St_est <- ts(St_est[as.vector(cycle(ts))], start=start(ts), freq=frequency(ts))
	St_est <- St_est - mean(ts, na.rm=TRUE)
	return(St_est)
	### Mean seasonal cycle of time series ts with the same length as ts, i.e. the mean seasonal cycle is repeated for each year. The mean seasonal cycle is centered to 0.
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)
ndvi.cycle <- MeanSeasonalCycle(ndvi)
plot(ndvi.cycle)

# the mean seasonal cycle is centered to 0 - add the mean of the time series if you want to overlay it with the original data:
plot(ndvi)
lines(ndvi.cycle + mean(ndvi, na.rm=TRUE), col="blue")
})
