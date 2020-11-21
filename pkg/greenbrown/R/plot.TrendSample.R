plot.TrendSample <- structure(function(
	##title<< 
	## Plot uncertainty of estimated trend dependent on start and end dates of time series
	
	##description<<
	## Plotting function for objects of class \code{\link{TrendSample}}. The function plots a point scatter plot defined by first year (x-axis) and last year (y-axis) of the time series. For each combination of first and last year a point symbol is plotted that represents the estimated trend. The size of the point indicates the absolute value of the trend slope. The color of the point indicates the trend slope direction (blue = negative trend, red = positive trend). The symbol of the point indicates that p-value of the Mann-Kendall trend test (snowflake: p <= 0.05, cross: 0.05 < p <= 0.1, circle: p > 0.1). Additionally, a second plot is added to the main plot (only if full = TRUE). This second plot is a scatter plot of trend slope against p-value (Mann-Kendall trend test) using the same points symbols as in the main plot. Thus the second plot can serve as a legend for the symbols used in the main plot. A boxplot on top of the second plot shows the distribution of the trend slope.
	
	x,
	### objects of class \code{\link{TrendSample}}
	
	y = "slope",
	### plot linear trend 'slope', 'perc' or 'tau' from Mann-Kendall trend test as response variable.
	
	full = TRUE,
	### make full plot or plot only main plot?
	
	...
	### further arguments to \code{\link{plot}}
	
	##seealso<<
	## \code{\link{TrendSample}}, \code{\link{TrendUncertainty}}		
) {
   op <- par()
	trd.ens <- x
	if (!(y %in% c("slope", "tau", "perc"))) y <- "slope"
	
	if (y == "slope" | y == "perc") {
	   xlab <- "Trend slope"
	   ylab <- "Slope p-value"
		if (y == "slope") {
		   xlab <- "Trend slope"
		   sl <- trd.ens$slope
		}
		if (y == "perc") {
		   xlab <- "Trend slope (%)"
		   sl <- trd.ens$perc
		}
		pval <- trd.ens$pval
		if (any(is.na(pval))) {
		   pval <- trd.ens$mk.pval
		   ylab <- "Mann-Kendall p-value"
		}
		test <- trd.ens$slope.test
		
	}
	if (y == "tau") {
	   xlab <- "Mann-Kendall tau"
	   ylab <- "Mann-Kendall p-value"
		sl <- trd.ens$mk.tau
		pval <- trd.ens$mk.pval
		test <- trd.ens$tau.test
	}

	sl.abs <- abs(sl)
	cex <- (sl.abs - min(sl.abs)) * ((4 - 0.3) / (max(sl.abs) - min(sl.abs))) + 0.3
	cols <- c("blue", "red")[cut(sl, c(min(sl), 0, max(sl)))]
	if (all(sl > 0)) cols <- "red"
	if (all(sl < 0)) cols <- "blue"
	pch <- c(8, 4, 1)[cut(pval, c(0, 0.05, 0.1, 1))]

	cex.def <- 1.3
	par(mfrow=c(1,1), mar=c(3.7, 3.5, 2, 0.5), mgp=c(2.4, 1, 0), cex=cex.def, cex.lab=cex.def*1.1, cex.axis=cex.def*1.1, cex.main=cex.def*1.1)
	par(fig=c(0, 1, 0, 1), new=FALSE)
	plot(trd.ens$start, trd.ens$end, col=cols, bg=cols, cex=cex, lwd=cex, pch=pch, xlab="First year", ylab="Last year", ...)
	
	# add text for number of years
	segments(x0=min(trd.ens$start), y0=seq(min(trd.ens$end), max(trd.ens$end)), x1=seq(max(trd.ens$start), min(trd.ens$start)), y1=max(trd.ens$end), lty=2, col="darkgrey")
	text(x=min(trd.ens$start), y=seq(min(trd.ens$end), max(trd.ens$end)), paste("n =", seq(min(trd.ens$length), max(trd.ens$length))), srt=44, pos=4, cex=0.7, col="darkgrey")

	if (full) {
		par(fig=c(0.5, 0.98, 0.1, 0.51), new=TRUE, cex.lab=cex.def*0.7, cex.axis=cex.def*0.7, cex.main=cex.def*0.8, mgp=c(1.3, 0.5, 0), mar=c(3.7, 3.5, 0, 0.5))
		xlim <- range(sl)
		plot(sl, pval, col=cols, bg=cols, cex=cex, lwd=cex, pch=pch, xlab=xlab, ylab=ylab, xlim=xlim)
		abline(v=0, h=c(0.05, 0.1), lty=2, lwd=0.04, col="darkgrey")

		par(fig=c(0.5, 0.98, 0.51, 0.56), new=TRUE, mar=c(0, 3.5, 0, 0.5))
		boxplot(sl, horizontal=TRUE, ylim=xlim, axes=FALSE, cex=0.7)
		points(y=1, x=sl[1], pch=pch[1], col=cols[1], cex=1)
		abline(v=0, lty=2, lwd=0.04, col="darkgrey")
		box()
		
		# plot significance of distribution
		sig <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
		col <- "blue"
		if (median(sl) >= 0) col <- "red"	
		text(x=median(sl, na.rm=TRUE), y=0.6, sig, pos=3, adj=0.5, col=col)
		
	}
	on.exit(par(op))
	
	# quantile(trd.ens$slope, c(0.05, 0.25, 0.5, 0.75, 0.95))
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# calculate uncertainty of trend dependent on start and end dates
ndvi <- aggregate(ndvi, FUN=mean)
trd.ens <- TrendSample(ndvi)
trd.ens

# plot relations between start, end dates, length and trend statistics
plot(trd.ens)
plot(trd.ens, response="tau")

})
