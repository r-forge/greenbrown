TrendRunmed <- structure(function(
	##title<< 
	## Trend estimation based on a running median
	
	##description<<
	## The function computes a non-linear trend based a running median.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	k=NULL,
	### integer width of median window; must be odd. If NULL a window size of 20 years (i.e. frequency * 20) will be used.
	
	...
	### additional arguments (currently not used)
	
	##seealso<<
	## \code{\link{stl}}
) {
	# get time series properties
	freq <- frequency(Yt)
	start <- start(Yt)
	end <- end(Yt)
	time <- time(Yt)	
	n <- length(Yt)

	if (is.null(k)) k <- freq * 10
	if (k %% 2 == 0) k <- k + 1
		
	# do initial linear interpolation and initial gap filling
	Na <- ts(is.na(Yt), start=start, end=end, frequency=freq)
	Yt2 <- c(rep(Yt[1:freq], n/freq), Yt, rep(Yt[(n-freq+1):n], n/freq))
	Tt <- runmed(Yt2, k)
	Tt <- ts(runmed(Tt[(n+1):(n+n)], 3), start=start, end=end, frequency=freq)
	Tt[Na == 1] <- NA	
	
	# results: pvalue with MannKendall test
	mk <- MannKendallSeg(Yt)[-1,]
	
	# return results
	result <- list(
		series = Yt,
		trend = Tt,
		time = as.vector(time),
		bp = NoBP(),
		slope = mk$lm.slope, 
		slope_unc = NoUnc(),
		slope_se = mk$lm.slope.se,
		pval = mk$lm.slope.pvalue,  
		perc = mk$lm.slope.perc,
		perc_unc = NoUnc(),
		mk.tau = mk$mk.tau,
		mk.tau_unc = NoUnc(),
		mk.pval = mk$mk.pval,
		bptest = NULL,
		method = "Runmed")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
}, ex=function(){
# calculate trend on mean annual NDVI values
trd <- TrendRunmed(ndvi)
trd
plot(trd)

})

