TSGFlinear <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling using linear interpolation
	##description<<
	## This function fills gaps in a time series by using linear interpolation \code{\link{na.approx}} and smoothes the time series by using running median window of size 3 \code{\link{runmed}}
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values by using \code{\link{na.spline}}?
	
	...
	### further arguments (currently not used)
	
	##seealso<<
	## \code{\link{TsPP}}

) {

	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	freq <- frequency(Yt)
	n <- length(Yt)
	start <- start(Yt)
	end <- end(Yt)
	mn <- min(Yt, na.rm=TRUE)
	mx <- max(Yt, na.rm=TRUE)
	
	# fill gaps with linear interpolation
	Ytf <- ts(na.approx(Yt), start=start, frequency=freq)
	
	# do smoothing
	sp <- runmed(Ytf, 3)	# running median
	Yt2 <- ts(sp, start=start, frequency=freq)
	Yt2 <- window(Yt2, start=start, end=end)
	
	# do interpolation
	if (interpolate) {
		Yt2 <- window(Yt2, start=c(start[1], 1), end=c(end[1], freq), extend=TRUE)
		intp <- approx(Yt2, xout=time(Yt2), rule=2)
		Yt2 <- ts(intp$y, start=c(start[1], 1), end=c(end[1], freq), frequency=freq)
		xoutts <- ts(NA, start=c(start[1], 1), end=c(end[1], 365), frequency=365)
		intp <- approx(Yt2, x=time(Yt2), xout=time(xoutts))	
		Yt3 <- ts(intp$y, start=start(xoutts), end=end(xoutts), frequency=frequency(xoutts))
	} else {
		Yt3 <- Yt2
	}
		
	# remove outliers
	Yt3[Yt3 < mn] <- mn
	Yt3[Yt3 > mx] <- mx
		
	return(Yt3)
	### The function returns a gap-filled and smoothed version of the time series.
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFlinear(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

})



