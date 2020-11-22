LmSeasonalCycle <- structure(function(
	##title<< 
	## Calculate the mean seasonal cycle of a time series based on a linear model
	
	##description<<
	## The function calculates the mean seasonal cycle of a time series based on a linear regression between the values and the time. Therefore a linear model with interactions is fitted to the original values Y of the form: Y = (a * m) * (b * sin(m)) * (c * cos(m)) + d where m are the the seasonal indices (e.g. months).  
	
	ts
	### univariate time series of class \code{\link{ts}}
		
	##seealso<<
	## \code{\link{Decompose}}, \code{\link{TrendSeasonalAdjusted}}, \code{\link{MeanSeasonalCycle}}
) {

	if (class(ts) != "ts") stop("ts should be of class ts.")
	time <- time(ts)
	years <- as.integer(time)
	mon <- (time - years)
	nyears <- length(unique(years))
	
	# estimate sesonal cycle based on linear model
   df <- data.frame(y=ts, x=mon, cos=cos(mon), sin=sin(mon))
   m <- lm(y ~ x * cos * sin, data=df)
   St_est <- ts(predict(m, df))
   tsp(St_est) <- tsp(ts)
   lim <- range(ts, na.rm=TRUE) * c(0.8, 1.2)
   St_est[St_est < lim[1]] <- lim[1]
   St_est[St_est > lim[2]] <- lim[2]
	St_est <- St_est - mean(ts, na.rm=TRUE)
	return(St_est)
	### Mean seasonal cycle of time series ts with the same length as ts, i.e. the mean seasonal cycle is repeated for each year. The mean seasonal cycle is centered to 0.
}, ex=function() {
ndvi.lmcycl <- LmSeasonalCycle(ndvi)
plot(ndvi.lmcycl)

ndvi.meancycl <- MeanSeasonalCycle(ndvi)
plot(ndvi.lmcycl[1:12], col="red", type="l")
lines(ndvi.meancycl[1:12], col="blue")

})
