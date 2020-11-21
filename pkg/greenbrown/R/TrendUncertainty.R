TrendUncertainty <- structure(function(
	##title<< 
	## Compute uncertainties in trend statistics according to different start and end dates
	
	##description<<
	## The function computes trend statistics (linear trend slope and intercept, Mann-Kendall tau and p-value) with associated uncertainties (standard deviation) by sampling the time series according to different start and end dates using the function \code{\link{TrendSample}}
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	seg = NULL,
	### a vector indicating segments of a time series. If NULL, provide bp
	
	bp = NoBP(),
	### detected breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}
	
	sample.method = c("sample", "all", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.75,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30,
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.
	
	fun.unc = NULL,
	### function to summarize the uncertainty of the trend (default: quantile 0.025 and 0.975). Can be also 'sd' or other functions.
	
	trend = TrendAAT
	### method that should be used to compute the trend
	
	##references<<  
		
	##seealso<<
	## \code{\link{Trend}}
) {

   if (is.null(seg)) {
	   # has the time series breakpoints?
	   has.bp <- !is.na(bp$breakpoints[1])

	   # get segments according to breakpoints
	   if (has.bp) {
		   seg <- breakfactor(bp)
	   } else {
		   seg <- factor(rep(1, length(Yt)))
	   }
	}
	seg <- as.numeric(seg)
	
	if (is.null(fun.unc)) {
		fun.unc <- function(x) quantile(x, c(0.025, 0.975, 0.5), na.rm=TRUE)
	}
	
   result <- llply(as.list(c(0, unique(seg))), function(s) {
	   if (s == 0) {
	      ti.seg <- time(Yt)
	      x <- Yt
	   } else {
	      ti.seg <- time(Yt)[seg == s]
	      x <- Yt[seg == s]
	   }
	   start <- ti.seg[1]
	   end <- ti.seg[length(ti.seg)]
	   Ytseg <- ts(x, start=start, end=end, frequency=frequency(Yt))
	   
		trd.stats <- TrendSample(Ytseg, sample.method=sample.method, trend=trend)
		pval_est <- trd.stats$pval[1]
		pval_unc <- do.call(fun.unc, list(x=trd.stats$pval))
		slope_est <- trd.stats$slope[1]
		slope_unc <- do.call(fun.unc, list(x=trd.stats$slope))
		tau_est <- trd.stats$mk.tau[1]
		tau_unc <- do.call(fun.unc, list(x=trd.stats$mk.tau))
		mkpval_est <- trd.stats$mk.pval[1]
		mkpval_unc <- do.call(fun.unc, list(x=trd.stats$mk.pval))
		perc_est <- trd.stats$perc[1]
		perc_unc <- do.call(fun.unc, list(x=trd.stats$perc))
		result <- list(s, start, end, pval_est, pval_unc, slope_est, slope_unc, perc_est, perc_unc, tau_est, tau_unc, mkpval_est, mkpval_unc)
		names(result) <- c("seg", "start", "end", "pval", "pval_unc", "slope", "slope_unc", "perc", "perc_unc", "mk.tau", "mk.tau_unc", "mk.pval", "mk.pval_unc")
		return(result)
	})
	
	return(result)
	### The function returns a data.frame with the estimated Mann-Kendall tau, p-value and slope and intercept of a linear trend with uncertainties defined as the standard deviation of these estimates dependent on different start and end dates.
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# aggregate time series to annual time steps
ndvi <- aggregate(ndvi, FUN=mean, na.rm=TRUE)
plot(ndvi)

# compute trend statistics dependent on start and end of the time series
trd.ens <- TrendSample(ndvi)
plot(trd.ens)

# compute statistics for trend
TrendUncertainty(ndvi)

# compute trend statistics with uncertainties by considering breakpoints
bp <- breakpoints(ndvi ~ time(ndvi))
trd.unc <- TrendUncertainty(ndvi, bp=bp)
trd.unc
trd.unc[[1]]$slope_unc


})
