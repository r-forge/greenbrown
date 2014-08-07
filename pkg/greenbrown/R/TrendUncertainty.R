TrendUncertainty <- structure(function(
	##title<< 
	## Compute uncertainties in trend statistics according to different start and end dates
	
	##description<<
	## The function computes trend statistics (linear trend slope and intercept, Mann-Kendall tau and p-value) with associated uncertainties (standard deviation) by sampling the time series according to different start and end dates using the function \code{\link{TrendSample}}
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	bp = NoBP(),
	### deetcted breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}
	
	sample.method = c("sample", "all", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.75,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.

	##references<<  
		
	##seealso<<
	## \code{\link{Trend}}
) {

	# has the time series breakpoints?
	has.bp <- !is.na(bp$breakpoints[1])

	# get segments according to breakpoints
	if (has.bp) {
		seg <- breakfactor(bp)
	} else {
		seg <- factor(rep(1, length(Yt)))
	}
	
	# get trend uncertainty from trend ensemble
	trd.stats <- aggregate(as.vector(Yt), by=list(seg), FUN=function(x) {
		trd.stats <- TrendSample(x, sample.method=sample.method)
		pval_est <- trd.stats$pvalue[1]
		pval_unc <- sd(trd.stats$pvalue, na.rm=TRUE)
		slope_est <- trd.stats$slope[1]
		slope_unc <- sd(trd.stats$slope, na.rm=TRUE)	
		tau_est <- trd.stats$tau[1]
		tau_unc <- sd(trd.stats$tau, na.rm=TRUE)	
		return(c(pval_est, pval_unc, slope_est, slope_unc, tau_est, tau_unc))
	})
	result <- as.data.frame(trd.stats$x)
	colnames(result) <- c("pval_est", "pval_unc", "slope_est", "slope_unc", "tau_est", "tau_unc")
	rownames(result) <- levels(seg)
	return(result)
	### The function returns a data.frame with the estimated Mann-Kendall tau, p-value and slope and intercept of a linear trend with uncertainties defined as the standard deviation of these estimates dependent on different start and end dates.
},ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# compute trend statistics with uncertainties
ndvi <- aggregate(ndvi, FUN=mean)
plot(ndvi)
TrendUncertainty(ndvi)

# compute trend statistics with uncertainties by considering breakpoints
ndvi <- aggregate(ndvi, FUN=mean)
bp <- breakpoints(ndvi ~ time(ndvi))
TrendUncertainty(ndvi, bp)

})