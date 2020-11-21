plot.TrendGradient <- structure(function(
	##title<< 
	## Plotting function for objects of class TrendGradient
	
	##description<<
	## This function plots a gradient of trend slopes (e.g. latitudinal gradient). 
	
	x,
	### Object of class 'TrendGradient' as returned from function \code{\link{TrendGradient}}
	
	type="xy",
	### plotting type: 'xy' = gradient at x axis and slope at y axis, 'yx' = gradient at y axis and slope at x axis.
	
	ylab=NULL,
	### A title for the y axis
	
	xlab=NULL,
	### A title for the x axis
	
	col="black",
	### line colors	
	
	ylim=NULL,
	### limits for y axis
	
	xlim=NULL,
	### limits for x axis
	
	add=FALSE,
	### add to exisiting plot?

	symbolic=TRUE,
	### Add p-value as symbols (TRUE) or not (FALSE). If TRUE the p-value of a trend slope is added as symbol to the plot. 
	
	symbols = "standard",
	### Type of symbols for p-values. "standard": *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1.; "simple": * (p <= 0.05), x (p < 0.1)
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{plot.default}}, \code{\link{plot.ts}}
) {	

	# get variables from trend gradient object
	x.df <- data.frame(x$zone, x$Slope, x$Pval, x$SlopeUncLower, x$SlopeUncUpper, x$SlopeUncMedian, x$LongestSEG, x$LengthSEG)
	# x.df <- na.omit(x.df)
	colnames(x.df) <- names(x)
	x <- x.df
	grad <- x$zone
	sl <- x$Slope
	sl.med <- x$SlopeUncMed
	pval <- x$Pval
	sl.unc1 <- x$SlopeUncLower
	sl.unc2 <- x$SlopeUncUpper
	
	# set x and y depending on plot type
	if (type == "xy") {
		x1 <- x2 <- x3 <- x4 <- grad
		y1 <- sl
		y2 <- sl.unc1
		y3 <- sl.unc2
		y4 <- sl.med
		if (is.null(xlab)) xlab <- "Latitude (N)"
		if (is.null(ylab)) ylab <- "NDVI trend (yr-1)"
	} 
	if (type == "yx") {
		x1 <- sl
		x2 <- sl.unc1
		x3 <- sl.unc2
		x4 <- sl.med
		y1 <- y2 <- y3 <- y4 <- grad
		if (is.null(xlab)) xlab <- "NDVI trend (yr-1)"
		if (is.null(ylab)) ylab <- "Latitude (N)"
	} 	
	
	# calculate limits
	if (is.null(ylim) & !add) ylim <- range(c(y1, y2, y3), na.rm=TRUE)
	if (is.null(xlim) & !add) xlim <- range(c(x1, x2, x3), na.rm=TRUE)
	
	# make color for uncertainty band
	col2 <- col2rgb(col)
	col2 <- rgb(col2[1,1], col2[2,1], col2[3,1], 50, maxColorValue=255)
	
	# make plot
	if (!add) {
		plot(x1, y1, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
		if (type == "xy") abline(h=0)
		if (type == "yx") abline(v=0)
	}
	if (type == "xy") PolygonNA(x=x2, lower=y2, upper=y3, col=col2)
	if (type == "yx") polygon(x=c(x2, rev(x3)), y=c(y2, rev(y3)), col=col2, border=col2)
	lines(x4, y4, lty=1, lwd=1, col=col)
	lines(x1, y1, lty=1, lwd=2, col=col)
	lines(x2, y2, lty=2, col=col)
	lines(x3, y3, lty=2, col=col)
	
	# add p-value as symbol
	if (symbolic) {
		if (symbols == "standard") {
			pval2 <- symnum(pval , corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
			text(x1, y1, pval2, col=col, font=2, cex=1.5)
		}
		if (symbols == "simple") {
			pval2 <- c(8, 4, 1)[cut(pval, c(0, 0.05, 0.1, 1))]
			points(x1, y1, pch=pval2, col=col, font=2, cex=1.4)
		}
	}

}, ex=function() {
# load a raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# compute a latitudinal gradient of trends (by default the method 'AAT' is used)
gradient <- TrendGradient(ndvimap, start=c(1982, 1), freq=12)
gradient
plot(gradient) 
# shown is the trend at each latitudinal band, the area represents the 95% 
# confidence interval of the trend (computed with function TrendUncertainty), 
# symbols indicate the p-value of the trend at each latitude

plot(gradient, type="yx") # the gradient can be also plotted in reversed order

# compute gradients with different trend methods
gradient.mac <- TrendGradient(ndvimap, start=c(1982, 1), freq=12, 
   method="SeasonalAdjusted", funSeasonalCycle=MeanSeasonalCycle)
plot(gradient.mac, col="blue", ylab="NDVI trend (month-1)")

# method AAT uses annual time steps, convert years -> months
gradient$Slope <- gradient$Slope / 12 
gradient$SlopeUncLower <- gradient$SlopeUncLower / 12
gradient$SlopeUncUpper <- gradient$SlopeUncUpper / 12
gradient$SlopeUncMedian <- gradient$SlopeUncMedian / 12
plot(gradient, col="red", add=TRUE)


})
