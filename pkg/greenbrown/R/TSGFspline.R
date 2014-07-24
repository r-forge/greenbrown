TSGFspline <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling using splines
	##description<<
	## This function fills gaps in a time series by using \code{\link{na.spline}} and smoothes the time series by using  \code{\link{smooth.spline}}
	
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
	
	# fill gaps with spline
	Ytf <- ts(na.spline(c(Yt, Yt, Yt))[(n+1):(n+1+n)], start=start, frequency=freq)
	
	# do smoothing
	sp <- smooth.spline(time(Ytf), Ytf)	# spline interpolation
	Yt2 <- ts(sp$y, start=start, frequency=freq)
	Yt2 <- window(Yt2, start=start, end=end)
	
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
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFspline(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

})



