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
	## \code{\link{ssa}}
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
	ssa <- Rssa::ssa(Yt1, kind="1d-ssa", L=as.integer(length(Yt1)*0.9))
	n <- Rssa::nu(ssa)
	ssarc <- Rssa::reconstruct(ssa, groups=as.list(1:n))
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
		method = "SSA")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
}, ex=function(){
## load a time series of NDVI (normalized difference vegetation index)
#data(ndvi)
#plot(ndvi)
#	
## calculate trend on mean annual NDVI values
#trd <- TrendSSA(ndvi)
#trd
#plot(trd)

})

