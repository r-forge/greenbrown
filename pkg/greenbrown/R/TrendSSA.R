TrendSSA <- structure(function(
	##title<< 
	## Trend estimation based on SSA (singluar spectrum analysis)
	
	##description<<
	## The function computes a non-linear trend based on \code{\link{ssa}}. Please note: Use the function \code{\link{TrendSeasonalAdjusted}} with the option funSeasonalCycle=SSASeasonalCylce to compute a linear trend with breakpoint detection based on a seasonal adjusted time series (method "SSA" as desribed in Forkel et al. 2013).
	
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
	xout <- time(Yt)
	Yt1 <- na.approx(Yt, xout=xout, rule=c(2,2))	
	Na1 <- na.approx(Na, xout=xout, method="constant", rule=c(2,2))	
	Yt1[Na1 == 1] <- Yt1[Na1 == 1] + rnorm(sum(Na1), 0, diff(range(Yt, na.rm=TRUE)) * 0.01)
	
	# perform a singular spectrum analysis on the data
	ssa <- ssa(Yt1, kind="1d-ssa", L=as.integer(length(Yt1)*0.9))
	ssarc <- Rssa:::reconstruct(ssa, groups=as.list(1:nu(ssa)))
	# plot(ssa, type="values"); plot(ssa, type="series"); plot(ssa, type="paired")
	
	# estimate frequency of each component
	ssarc <- ssarc[unlist(llply(ssarc, function(ts) !AllEqual(ts)))]
	
	# reconstruct time series from all components with very low to seasonal frequencies
	ssa.ts <- ssarc[1]	
	if (length(ssa.ts) > 0) {
		Tt <- ts(rowSums(as.data.frame(ssa.ts)), start=start, end=c(end[1], freq), frequency=freq)
	} else {
		Tt <- ts(NA, start, end=c(end[1], freq), frequency=freq)
	}
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
		method = "SSA")
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
trd <- TrendSSA(ndvi)
trd
plot(trd, symbolic=TRUE)

})

