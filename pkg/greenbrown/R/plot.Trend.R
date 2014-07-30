plot.Trend <- structure(function(
	##title<< 
	## Create time series plots with trend lines
	
	##description<<
	## This is the standard plot function for results of the \code{\link{Trend}} function. See \code{\link{plot.default}} for further specifications of basic plots.
	
	x,
	### Object of class 'Trend' as returned from function \code{\link{Trend}}
	
	ylab="NDVI",
	### A title for the y axis
	
	add=FALSE,
	### add to exisiting plot
	
	col=c("black", "blue", "red", "blue"),
	### colors for (1) time series, (2) trend line, (3) breakpoints and (4) trend uncertainty
	
	lty=c(2, 1, 2, 3),
	### line types for (1) time series, (2) trend line, (3) breakpoints and (4) trend uncertainty
	
	lwd=1,
	## line width
	
	symbolic=TRUE,
	### add significance as symbols (TRUE) or as legend text (FALSE). If TRUE the p-value of a trend slope is added as symbol as following: *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1. 
	
	uncertainty=TRUE,
	### plot uncertainty in trend slopes? (only possible if the x 'Trend' object includes uncertainty estimates)
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{plot.default}}, \code{\link{plot.ts}}
) {
	if (length(col) != 4) col <- rep(col[1], 4)
	if (length(lty) != 4) lty <- rep(lty[1], 4)

	# plot time series
	if (!add) {
		plot(x$series, type="l", xaxt="n", xlab="", col=col[1], lty=lty[1], ylab=ylab, lwd=lwd) #, ...)
		axis(1, pretty(x$time), pretty(x$time))
	}
	if (add) lines(x$series, col=col[1], lty=lty[1], lwd=lwd)
	lines(x$trend, col=col[2], lty=lty[2], lwd=lwd*2) #, ...)
			
	# add lines for breakpoints with confidence intervalls
	if (!is.na(x$bp$breakpoints[1])) {
		if (symbolic) points(x=x$time[x$bp$breakpoints], y=x$trend[x$bp$breakpoints], col=col[3], pch=3, cex=1.2)
		if (!symbolic) abline(v=x$time[x$bp$breakpoints], col=col[3], lty=lty[3])
		ci <- confint(x$bp)$confint
		for (i in 1:length(ci)) {
			ti <- ci[i]
			ti[ti == 0] <- 1
			ci[i] <- x$time[ti]
		} 
		for (i in 1:nrow(ci)) lines(x=ci[i,], y=rep(x$trend[x$bp$breakpoints[i]], 3), col=col[3])
	}	
	
	# add legend or symbolic representation of significance
	pval.symbol <- rep("", length(x$pval))
	pval.symbol[!is.na(x$pval)] <- symnum(x$pval[!is.na(x$pval)] , corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
	xpos <- NULL
	ypos <- NULL
	if (!is.na(x$bp$breakpoints[1])) { 
		seg <- breakfactor(x$bp)
	} else {
		seg <- factor(rep(1, length(x$trend)))
	}
	for (i in 1:length(unique(seg))) {
		
		# calculate position of symbols
		d <- x$time[seg == unique(seg)[i]]
		d <- d[!is.na(d)]
		xpos <- c(xpos, mean(d, na.rm=TRUE)[1])
		d <- x$trend[seg == unique(seg)[i]]
		d <- d[!is.na(d)]
		ypos <- c(ypos, quantile(d, prob=0.6, na.rm=TRUE)[1])
		
		# calculate range of trend slope uncertainty
		if (!is.na(x$slope_unc[i]) & uncertainty) {
			t <- x$time[seg == unique(seg)[i]]
			y <-  x$trend[seg == unique(seg)[i]]
			y1 <- (x$slope[i] + x$slope_unc[i]) * 1:length(t)
			y1 <- y1 - mean(y1, na.rm=TRUE) + mean(y, na.rm=TRUE)
			y2 <- (x$slope[i] - x$slope_unc[i]) * 1:length(t)
			y2 <- y2 - mean(y2, na.rm=TRUE) + mean(y, na.rm=TRUE)
			lines(t, y1, col=col[2], lty=lty[4], lwd=lwd)
			lines(t, y2, col=col[2], lty=lty[4], lwd=lwd)
		}
	}
	 
	if (symbolic) {	
		text(xpos, ypos, pval.symbol, col=col[2], font=2, cex=2)				
	} else {
		legend("bottomleft", paste("Slope = ", signif(x$slope, 2), " (p-value = ", signif(x$pval, 2), ")", sep=""), bty="n", text.col="blue")	
	}
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
plot(ndvi.trend)
plot(ndvi.trend, uncertainty=FALSE)

ndvi.trend.aat <- Trend(ndvi, method="AAT", mosum.pval=1)
plot(ndvi.trend.aat)
plot(ndvi.trend.aat, symbolic=FALSE)
plot(ndvi.trend.aat, symbolic=FALSE, uncertainty=FALSE)

ndvi.trend.stm <- Trend(ndvi, method="STM", mosum.pval=1)
plot(ndvi.trend.stm)

plot(ndvi.trend.aat, symbolic=TRUE, ylim=c(0.23, 0.31), col=c("blue", "blue", "red"))
plot(ndvi.trend.stm, symbolic=TRUE, col=c("darkgreen", "darkgreen", "red"), lty=c(0, 1, 1), add=TRUE)
	
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
	trd <- format(signif(x$slope, 3), digits=3, scientific=FALSE, width=8)
	trd_unc <- format(signif(x$slope_unc, 3), digits=3, scientific=FALSE, width=8)
	tau <- format(signif(x$tau, 3), digits=3, scientific=FALSE, width=8)
	tau_unc <- format(signif(x$tau_unc, 3), digits=3, scientific=FALSE, width=8)
	pval <- format(signif(x$pval, 3), digits=3, width=8)
	if (is.null(x$bptest) & !is.null(x$tau)) trd <- format(signif(x$tau, 3), digits=3, scientific=FALSE, width=8)
	names <- format(c("slope", "unc", "p-value", "tau", "unc"), justify="right", width=8)
	
	if (nseg > 1) bp.dates <- format(x$time[x$bp$breakpoints], justify="right", width=8)

	# print time series information
	cat("--- Trend ---------------------------------------", "\n")
	cat("Calculate trends and trend changes on time series", "\n")
	cat("-------------------------------------------------", "\n")
	cat("\n")
	cat("list of class 'Trend' with the following components:", "\n")
	cat(  "$series    : time series on which the trend was calculated.", "\n")
    cat(  "$trend     : time series with the estimated trend.", "\n")
    cat(  "$time      : a vector of time steps.", "\n")
    cat(  "$bp        : an object of class breakpoints.", "\n")
    cat(  "$slope     : a vector of the trend slopes for each trend segment.", "\n")
	cat(  "$slope_unc : a vector of the uncertainty (standard deviation) of trend slopes.", "\n")
    cat(  "$pval      : a vector of the p-values of the trend for each trend segment.", "\n")
	cat(  "$tau       : a vector of Mann-Kendall trend test tau values.", "\n")
	cat(  "$tau_unc   : a vector of the uncertainty (standard deviation) of Mann-Kendall trend test tau values.", "\n")
	cat("\n")
	cat("Time series start   : ", paste(start(x$series), sep="-"), "\n")
	cat("Time series end     : ", paste(end(x$series), sep="-"), "\n")
	cat("Time series length  : ", length(x$series), "\n")
	cat("\n")
	if (!is.null(x$bptest)) {
		cat("OLS-based MOSUM test for structural change", "\n")
		cat("  statistic         :", x$bptest$statistic, "\n")
		cat("  p-value           :", x$bptest$p.value, "\n")
		cat("\n")
	}
	cat("Trend method        :", x$method, "\n")
	if (x$method == "STM") {
		cat("  breakpoint type   : ", x$bptype, "\n")
	}
	cat("\n")
	
	# print coefficients of piecewise linear trend
	if (nseg > 1) cat("Trend in segments of the time series", "\n")
	if (!is.null(x$bptest)) {
		if (nseg > 1) cat("  breakpoints       :", paste(bp.dates, sep="  "), "\n")
		if (nseg == 1) cat("  breakpoints       : no breakpoints were detected", "\n")
	}
	cat("  trend             :", "\n")
	if (!is.null(x$bptest)) {
		cat("    ", "              ", names, "\n")
		for (i in 1:nseg) cat("    ", "segment", format(i, width=3, justify="right"), ":", trd[i], trd_unc[i], pval[i], tau[i], tau_unc[i], "\n")		
	} else {
		cat("    ", "Mann-Kendall test  ", "\n")
		cat("    ", "  tau     :", trd, "\n")		
		cat("    ", "  p-value :", pval, "\n")		
	}
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
summary(ndvi.trend)
	
})

