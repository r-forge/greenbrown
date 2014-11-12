TrendUncertainty <- structure(function(
	##title<< 
	## Compute uncertainties in trend statistics according to different start and end dates
	
	##description<<
	## The function computes trend statistics (linear trend slope and intercept, Mann-Kendall tau and p-value) with associated uncertainties (standard deviation) by sampling the time series according to different start and end dates using the function \code{\link{TrendSample}}
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	bp = NoBP(),
	### detected breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}
	
	sample.method = c("sample", "all", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.75,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30,
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.
	
	fun.unc = NULL
	### function to summarize the uncertainty of the trend (default: quantile 0.05 and 0.95). Can be also 'sd' or other functions.
	
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
	
	if (is.null(fun.unc)) {
		fun.unc <- function(x) quantile(x, c(0.025, 0.975, 0.5), na.rm=TRUE)
	}
	
	# get trend uncertainty from trend ensemble
	result <- by(Yt, list(seg), function(x) {
		trd.stats <- TrendSample(x, sample.method=sample.method)
		pval_est <- trd.stats$pvalue[1]
		pval_unc <- do.call(fun.unc, list(x=trd.stats$pvalue))
		slope_est <- trd.stats$slope[1]
		slope_unc <- do.call(fun.unc, list(x=trd.stats$slope))
		tau_est <- trd.stats$tau[1]
		tau_unc <- do.call(fun.unc, list(x=trd.stats$tau))
		result <- list(pval_est, pval_unc, slope_est, slope_unc, tau_est, tau_unc)
		names(result) <- c("pval", "pval_unc", "slope", "slope_unc", "tau", "tau_unc")
		return(result)
	})
	
	return(result)
	### The function returns a data.frame with the estimated Mann-Kendall tau, p-value and slope and intercept of a linear trend with uncertainties defined as the standard deviation of these estimates dependent on different start and end dates.
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# aggregate time series to annual time steps
ndvi <- aggregate(ndvi, FUN=mean)
plot(ndvi)

# compute trend statistics dependent on start and end of the time series
trd.ens <- TrendSample(ndvi)
plot(trd.ens)

# compute statistics for trend
TrendUncertainty(ndvi)

# compute trend statistics with uncertainties by considering breakpoints
bp <- breakpoints(ndvi ~ time(ndvi))
trd.unc <- TrendUncertainty(ndvi, bp)
trd.unc
trd.unc[[1]]$slope_unc


})