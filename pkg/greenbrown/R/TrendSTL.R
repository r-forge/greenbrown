TrendSTL <- structure(function(
	##title<< 
	## Trend estimation based on STL (Seasonal Decomposition of Time Series by Loess)
	
	##description<<
	## The function computes a non-linear trend based on \code{\link{stl}}.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	...
	### additional arguments (currently not used)
	
	##seealso<<
	## \code{\link{stl}}
) {
	time <- time(Yt)	
	
	# do initial linear interpolation and initial gap filling
	Na <- ts(is.na(Yt), start=start(Yt), end=end(Yt), frequency=frequency(Yt))
	xout <- time(Yt)
	Yt1 <- na.approx(Yt, xout=xout, rule=c(2,2))	
	Na1 <- na.approx(Na, xout=xout, method="constant", rule=c(2,2))	
	Yt1[Na1 == 1] <- Yt1[Na1 == 1] + rnorm(sum(Na1), 0, diff(range(Yt, na.rm=TRUE)) * 0.01)
	
	# get trend component from stl
	stl <- stl(Yt1, s.window="periodic")
	Tt <- stl$time.series[,2]
	Tt[Na1 == 1] <- NA
	
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
		pval = mk.pval,
		mk.tau = mk.tau,
		bptest = NULL,
		method = "STL")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend" with the following components:
	### \itemize{ 
	### \item{ \code{series} time series on which the trend was calculated. }
	### \item{ \code{trend} time series with the estimated trend component. }
	### \item{ \code{time} a vector of time steps. }
	### \item{ \code{pval} Mann-Kendall test p-value of the trend component. }
	### }
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd <- TrendSTL(ndvi)
trd
plot(trd, symbolic=TRUE)

})

