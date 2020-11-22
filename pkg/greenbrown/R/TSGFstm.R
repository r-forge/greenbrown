TSGFstm <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling based on a season-trend model
	##description<<
	## This function fills gaps in a time series by using a season-trend model as in \code{\link{TrendSTM}} (Verbesselt et al. 2010, 2012).
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values by using \code{\link{na.spline}}?
	
	...
	### further arguments to \code{\link{TrendSTM}}.
	
	##references<<
	## Verbesselt, J.; Hyndman, R.; Zeileis, A.; Culvenor, D., Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment 2010, 114, 2970-2980. \cr
	## Verbesselt, J.; Zeileis, A.; Herold, M., Near real-time disturbance detection using satellite image time series. Remote Sensing of Environment 2012, 123, 98-108.
	
	##seealso<<
	## \code{\link{TsPP}}, \code{\link{TrendSTM}} 

) {

	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	freq <- frequency(Yt)
	n <- length(Yt)
	start <- start(Yt)
	end <- end(Yt)
	mn <- min(Yt, na.rm=TRUE)
	mx <- max(Yt, na.rm=TRUE)
	
	# get fit from STM
	Yt2 <- TrendSTM(Yt, ...)$fit
	
	# do interpolation
	if (interpolate) {
		xoutts <- ts(NA, start=start, end=c(end[1], 365), frequency=365)
		xout <- time(xoutts)
		intp <- na.spline(Yt2, xout=xout)	
		Yt3 <- ts(intp, start=start, end=end(xoutts), frequency=frequency(xoutts))
	} else {
		Yt3 <- Yt2
	}
		
	# remove outliers
	Yt3[Yt3 < mn] <- mn
	Yt3[Yt3 > mx] <- mx
		
	return(Yt3)
	### The function returns a gap-filled and smoothed version of the time series.
}, ex=function() {
# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFstm(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled")
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

})



