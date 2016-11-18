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
	Tt <- ts(predict(m, df), start=start, end=end, frequency=freq)
	
	# results: pvalue with MannKendall test
	mk <- MannKendall(Tt)
	mk.pval <- mk$sl 
	mk.tau <- mk$tau
	slope_unc <- data.frame(.id=1, NA, NA, NA)
	pval_unc <- data.frame(.id=1, NA, NA, NA)
	tau_unc <- data.frame(.id=1, NA, NA, NA)
	
	# return results
	result <- list(
		series = Yt,
		trend = Tt,
		time = as.vector(time),
		bp = NoBP(),
		slope = NA,
		slope_unc = slope_unc,
		pval = mk.pval,
		pval_unc = pval_unc,
		tau = mk.tau,
		tau_unc = tau_unc,
		bptest = NULL,
		method = "Polynomial")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd <- TrendPoly(ndvi)
trd
plot(trd)

})

