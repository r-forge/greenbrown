plot.Trend <- structure(function(
	##title<< 
	## Create time series plots with trend lines
	
	##description<<
	## This is the standard plot function for results of the \code{\link{Trend}} function. See \code{\link{plot.default}} for further specifications of basic plots.
	
	x,
	### Object of class 'Trend' as returned from function \code{\link{Trend}}
	
	ylab="NDVI",
	### A title for the y axis
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{plot.default}}, \code{\link{plot.ts}}
) {

	# plot time series
	plot(x$series, type="l", xaxt="n", xlab="", ylab=ylab, ...)
	lines(x$trend, col="blue", ...)
	
	# add lines for confidence intervalls
	if (!is.na(x$bp$breakpoints[1])) {
		abline(v=x$time[x$bp$breakpoints], col="red", lty=2)
		ci <- confint(x$bp)$confint
		for (i in 1:length(ci)) {
			ti <- ci[i]
			ti[ti == 0] <- 1
			ci[i] <- x$time[ti]
		} 
		for (i in 1:nrow(ci)) lines(x=ci[i,], y=rep(min(x$series), 3), col="red")
	}
		
	axis(1, pretty(x$time), pretty(x$time))
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
summary(ndvi.trend)
plot(ndvi.trend)
	
})


print.Trend <- summary.Trend <- structure(function(
	##title<< 
	## Print a summary of calculated trends
	
	##description<< The function prints a summary the results from \code{\link{Trend}}.
	
	x
	### Object of class 'Trend' as returned from function \code{\link{Trend}}
	
) {
	nseg <- length(x$slope)
	# get coefficients for piecewise linear trend
	trd <- x$slope
	trd <- format(trd, digits=3, scientific=FALSE, width=8)
	pval <- format(x$pval, digits=3, scientific=FALSE, width=8)
	names <- format(c("slope", "p-value"), justify="right", width=8)
	
	if (nseg > 1) bp.dates <- format(x$time[x$bp$breakpoints], justify="right", width=8)

	# print time series information
	cat("--- Trend ---------------------------------------", "\n")
	cat("Calculate trends and trend changes on time series", "\n")
	cat("-------------------------------------------------", "\n")
	cat("\n")
	cat("list of class 'Trend' with the following components:", "\n")
	cat(  "$series : time series on which the trend was calculated.", "\n")
    cat(  "$trend  : time series with the estimated trend.", "\n")
    cat(  "$time   : a vector of time steps.", "\n")
    cat(  "$bp     : an object of class breakpoints.", "\n")
    cat(  "$slope  : a vector of the trend slopes for each trend segment.", "\n")
    cat(  "$pval   : a vector of the p-values of teh trend for each trend segment.", "\n")
	cat("\n")
	cat("Time series start   : ", paste(start(x$series), sep="-"), "\n")
	cat("Time series end     : ", paste(end(x$series), sep="-"), "\n")
	cat("Time series length  : ", length(x$series), "\n")
	cat("\n")
	cat("OLS-based MOSUM test for structural change", "\n")
	cat("  statistic         : ", x$bptest$statistic, "\n")
	cat("  p-value           : ", x$bptest$p.value, "\n")
	cat("\n")
	cat("Trend method        : ", x$method, "\n")
	if (x$method == "STM") {
		cat("  breakpoint type   : ", x$bptype, "\n")
	}
	cat("\n")
	
	# print coefficients of piecewise linear trend
	if (nseg > 1) cat("Trend in segments of the time series", "\n")
	if (nseg > 1) cat("  breakpoints       :", paste(bp.dates, sep="  "), "\n")
	if (nseg > 1) cat("  breakpoints       :", paste(bp.dates, sep="  "), "\n")
	if (nseg == 1) cat("  breakpoints       : no breakpoints were detected", "\n")
	cat("  trend             :", "\n")
	cat("    ", "segment      ", names, "\n")
	for (i in 1:nseg) cat("    ", "segment", format(i, width=3, justify="right"), ":", trd[i], pval[i], "\n")		
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
summary(ndvi.trend)
plot(ndvi.trend)
	
})

