TSGFssa <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling using singular spectrum analysis
	##description<<
	## This function fills gaps and smoothes a time series by using 1-dimensional singular spectrum analysis.
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values?
	
	...
	### further arguments (currently not used)
	
	##seealso<<
	## \code{\link{TsPP}}

) {
	require(Rssa)
	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	freq <- frequency(Yt)
	start <- start(Yt)
	end <- end(Yt)
	mn <- min(Yt, na.rm=TRUE)
	mx <- max(Yt, na.rm=TRUE)
	
	# do initial linear interpolation and initial gap filling
	Na <- ts(is.na(Yt), start=start, end=end, frequency=freq)
	if (interpolate) {
		xoutts <- ts(NA, start=start, end=c(end[1], 365), frequency=365)
		xout <- time(xoutts)
		freq.out <- 365
	} else {
		xout <- time(Yt)
		freq.out <- freq
	}
	Yt1 <- na.approx(Yt, xout=xout, rule=c(2,2))	
	Na1 <- na.approx(Na, xout=xout, method="constant", rule=c(2,2))	
	Yt1[Na1 == 1] <- Yt1[Na1 == 1] + rnorm(sum(Na1), 0, diff(range(Yt, na.rm=TRUE)) * 0.01)
	
	if (AllEqual(Yt1)) return(Yt1)

	# perform a singular spectrum analysis on the data
	ssa <- ssa(Yt1, kind="1d-ssa", L=as.integer(length(Yt1)*0.9))
	ssarc <- Rssa:::reconstruct(ssa, groups=as.list(1:nu(ssa)))
	# plot(ssa, type="values"); plot(ssa, type="series"); plot(ssa, type="paired")
	
	# estimate frequency of each component
	ssarc <- ssarc[unlist(llply(ssarc, function(ts) !AllEqual(ts)))]
	ssafreq <- unlist(llply(ssarc, .fun=Seasonality, return.freq=TRUE))
	
	# reconstruct time series from all components with low to seasonal frequencies
	ssa.ts <- ssarc[ssafreq < 1.5]	
	if (length(ssa.ts) > 0) {
		Yt2 <- ts(rowSums(as.data.frame(ssa.ts)), start=start, end=c(end[1], freq.out), frequency=freq.out)
	} else {
		Yt2 <- ts(NA, start, end=c(end[1], freq.out), frequency=freq.out)
	}
		
	# remove outliers
	Yt2[Yt2 < mn] <- mn
	Yt2[Yt2 > mx] <- mx
		
	return(Yt2)
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
tsgf <- TSGFssa(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], window(tsgf[is.na(gaps)], end=c(2008, 11)), xlab="original", ylab="gap filled")
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

})



