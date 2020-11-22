TrendPoly <- structure(function(
	##title<< 
	## Trend estimation based on a 4th order polynomial
	
	##description<<
	## The function computes a trend based on a 4th order polynomial function.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
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
	
	# do initial linear interpolation and initial gap filling
	Na <- ts(is.na(Yt), start=start, end=end, frequency=freq)
	
	# compute polynomial
	df <- data.frame(Yt=Yt, time=time)
	m <- lm(Yt ~ I(time^4) + I(time^3) + I(time^2) + time, data=df)
	Tt <- suppressWarnings(ts(predict(m, df), start=start, end=end, frequency=freq))
	
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
		method = "Polynomial")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
}, ex=function(){
# calculate trend on mean annual NDVI values
trd <- TrendPoly(ndvi)
trd
plot(trd)

})

