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
	
	# return results
	result <- list(
		series = Yt,
		trend = Tt,
		time = as.vector(time),
		bp = NoBP(),
		slope = NA,
		slope_unc= NA,
		pval = mk.pval,
		pval_unc = NA,
		tau = mk.tau,
		tau_unc = NA,
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

