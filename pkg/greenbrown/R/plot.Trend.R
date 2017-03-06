plot.Trend <- structure(function(
	##title<< 
	## Plot trend and breakpoint results
	
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
	### add significance as symbols (TRUE). If TRUE the p-value of a trend slope is added as symbol as following: *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1. 
	
	legend=FALSE,
	### add slope and p-value as legend
	
	axes=TRUE,
	### plot axes?
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{plot.default}}, \code{\link{plot.ts}}
) {
	if (length(col) != 4) col <- rep(col[1], 4)
	if (length(lty) != 4) lty <- rep(lty[1], 4)
	
	if (symbolic | legend) {
	  if (!(x$method == "STM" | x$method == "AAT" | x$method == "SeasonalAdjusted" | grepl("RQ", x$method))) {
	      symbolic <- FALSE
	      legend <- FALSE
	  } 
	}

	# plot time series
	if (!add) {
		yaxt <- "s"
		if (!axes) yaxt <- "n"
		plot(x$series, type="l", xaxt="n", xlab="", col=col[1], lty=lty[1], ylab=ylab, lwd=lwd, ...)
		if (axes) axis(1, pretty(x$time), pretty(x$time))
	}
	if (add) lines(x$series, col=col[1], lty=lty[1], lwd=lwd)
	lines(x$trend, col=col[2], lty=lty[2], lwd=lwd*2, ...)
			
	# add lines for breakpoints with confidence intervalls
	if (!is.na(x$bp$breakpoints[1])) {
		if (symbolic) points(x=x$time[x$bp$breakpoints], y=x$trend[x$bp$breakpoints], col=col[3], pch=3, cex=1.2)
		if (!symbolic) abline(v=x$time[x$bp$breakpoints], col=col[3], lty=lty[3])
		ci <- try(confint(x$bp)$confint, silent=TRUE)
		if (class(ci) != "try-error") {
		   for (i in 1:length(ci)) {
			   ti <- ci[i]
			   ti[ti == 0] <- 1
			   ci[i] <- x$time[ti]
		   } 
		   for (i in 1:nrow(ci)) lines(x=ci[i,], y=rep(x$trend[x$bp$breakpoints[i]], 3), col=col[3])
		}
	}	
	
	# add legend or symbolic representation of significance
	pval.symbol <- rep("", length(x$pval))
	pval.symbol[!is.na(x$pval)] <- symnum(x$mk.pval[!is.na(x$mk.pval)] , corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
	xpos <- NULL
	ypos <- NULL
	if (!is.na(x$bp$breakpoints[1])) { 
		seg <- breakfactor(x$bp)
	} else {
		seg <- factor(rep(1, length(x$trend)))
	}
	for (i in 1:length(unique(seg))) {
		
		# calculate position of symbols
		b <- is.na(x$series)
		d <- (x$time[!b])[seg == unique(seg)[i]]
		xpos <- c(xpos, mean(d, na.rm=TRUE)[1])
		d <- (x$trend[!b])[seg == unique(seg)[i]]
		ypos <- c(ypos, quantile(d, prob=0.6, na.rm=TRUE)[1])
		
#		# add trend slope uncertainty
#		if ((!is.null(x$slope_se[i]) | !is.null(x$slope_unc[i])) & uncertainty) {
#			t <- x$time[seg == unique(seg)[i]]
#			y <-  x$trend[seg == unique(seg)[i]]
#			if (!is.null(x$slope_unc[i])) {
#			   unc <- unlist(x$slope_unc[i])
#			} else {
#			   unc <- unlist(x$slope_se[i])
#			}
#			y1 <- (x$slope[i] + unc[1]) * 1:length(t)
#			y2 <- (x$slope[i] - unc[1]) * 1:length(t)
#			y1 <- y1 - mean(y1, na.rm=TRUE) + mean(y, na.rm=TRUE)
#			y2 <- y2 - mean(y2, na.rm=TRUE) + mean(y, na.rm=TRUE)
#			lines(t, y1, col=col[2], lty=lty[4], lwd=lwd)
#			lines(t, y2, col=col[2], lty=lty[4], lwd=lwd)
#		}
	}
	 
	if (symbolic) {	
		text(xpos, ypos, pval.symbol, col=col[2], font=2, cex=2)				
	} 
	if (legend) {
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

ndvi.trend.aat <- Trend(ndvi, method="AAT", mosum.pval=1)
plot(ndvi.trend.aat)
plot(ndvi.trend.aat, symbolic=FALSE)
plot(ndvi.trend.aat, symbolic=FALSE, legend=TRUE)

ndvi.trend.stm <- Trend(ndvi, method="STM", mosum.pval=1)
plot(ndvi.trend.stm)

plot(ndvi.trend.aat, symbolic=TRUE, ylim=c(0.23, 0.31), 
   col=c("blue", "blue", "red"))
plot(ndvi.trend.stm, symbolic=TRUE, col=c("darkgreen", "darkgreen", "red"), 
	lty=c(0, 1, 1), add=TRUE)
	
})


print.Trend <- structure(function(
	##title<< 
	## Prints trends
	
	##description<< The function prints an object of class \code{\link{Trend}}.
	
	x,
	### Object of class 'Trend' as returned from function \code{\link{Trend}}
	
	...
	### further arguments (not used)
	
) {
	nseg <- length(x$slope)
	# get coefficients for piecewise linear trend
	slope <- format(signif(x$slope, 3), width=10)
	pval <- format(signif(x$pval, 3), width=10)
	mk.tau <- format(signif(x$mk.tau, 3), width=10)
	mk.pval <- format(signif(x$mk.pval, 3), width=10)
	perc <- format(signif(x$perc, 3), width=10)
	names <- format(c("segment", "slope", "%", "p-value", "MK tau", "MK p-value"), justify="right", width=10)
		
	if (nseg > 1) bp.dates <- format(x$time[x$bp$breakpoints], justify="right", width=8)

	# print time series information
	cat("--- Trend ---------------------------------------", "\n")
	cat("Calculate trends and trend changes on time series", "\n")
	cat("-------------------------------------------------", "\n")
	cat("\n")
	cat("Time series start   : ", paste(start(x$series), sep="-"), "\n")
	cat("Time series end     : ", paste(end(x$series), sep="-"), "\n")
	cat("Time series length  : ", length(x$series), "\n")
	cat("\n")
	
	cat("Trend method        :", x$method, "\n")
	if (x$method == "STM" & nseg > 1) {
		cat("  breakpoint type   : ", x$bptype, "\n")
	}
	cat("\n")
	
	if (x$method == "STM" | x$method == "AAT" | x$method == "SeasonalAdjusted" | grepl("RQ", x$method)) {
		cat("Test for structural change", "\n")
	   if (!is.null(x$bptest)) {
		   cat("  OLS-based MOSUM test for structural change", "\n")
		   cat("    statistic       :", x$bptest$statistic, "\n")
		   cat("    p-value         :", x$bptest$p.value, "\n")
	   } else {
		   cat("  Test for structural change was not perfomed.", "\n")
	   }
	   if (!is.null(x$bptest)) {
		   if (nseg > 1) cat("  Breakpoints       :", paste(bp.dates, sep="  "), "\n")
		   if (nseg == 1) cat("  Breakpoints       : Breakpoints were not detected.", "\n")
	   }	
	   cat("\n")
	
	   # print coefficients of piecewise linear trend
	   cat("Trends in segments of the time series", "\n")
	   cat(" ", names, "\n")
	   for (s in 1:nseg) cat(" ", format(s, width=10), slope[s], perc[s], pval[s], mk.tau[s], mk.pval[s], "\n")
	}
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
	
})

