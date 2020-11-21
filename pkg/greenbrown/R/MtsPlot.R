MtsPlot <- structure(function(
	##title<< 
	## Multiple time series plot
	##description<< 
	## Allows to plot multiple time series with different y-axes into one plot. This function is an alternative approach to plot trends than \code{\link{plot.Trend}}. It is especially suited to plot trends of different variables with different units in one figure.
	
	x,
	### time series that should be plotted. If x has multiple columns, uncertainties can be plotted as individual 'lines' or as 'polygon' (see 'unc').
	
	xlim = NULL, 
	### range of x-axis
	
	rge = c(0, 1),
	### range of y-axis in which the time series should be plotted. 
	
	ylim = NULL,
	### axis limits of the y-axis for the data in x
	
   col = "blue",
   ### color of the time series and corresponding axis
   
   lwd = 1,
   ### line width of the time series
	
	add = FALSE,
	### initialize a new plot if FALSE or adds time series to existing plot if TRUE
	
	axis = TRUE,
	### draws a y-axis if TRUE
	
	ylab = "x",
	### label for y-axis
	
	axis.pos = "l",
	### position of y-axis: l (left) or r (right) 
	
	axis.col = col,
	### color for the axis
	
	axis.lim = NULL,
	### limits (min/max) for the axis
	
	trend = TrendAAT,
	### compute and adds a trend line using \code{\link{TrendText}}. Set to NULL for no trends.
	
	trend.period = NULL,
	### define a sub-period (e.g. c(1990, 2015)) to compute trends for the full time series and this period
	
	trend.text = 1,
	### type of trend text to plot: 1 (trend in %), 2 (trend slope with unit), 3 (trend slope without unit), 4 (p-value from Mann-Kendall trend test), 5 (only if x has multiple columns: p-value Wilcoxon rank sum test). In case of other values, no text will be plotted. In case of trend.text=5, the Wilcoxon rank sum test is used to test if the ensmble of trend slopes for each column in x is significant different from 0.
	
	unit = "",
	### unit of the values in x (used for trend text)
	
	text = "",
	### additional text that should be plotted next to each time series
	
	unc = "polygon",
	### if x has multiple columns, plot uncertainty ranges as 'polygon' or line'?
	
	ensfun = mean,
	### if x has multiple columns, how to compute the main estimate of the emsemble?
	
	... 
	### further arguments to \code{\link{plot.default}}
	
	##details<< 
	## This function plots a scatterplot.
	
	##seealso<< 
	## \code{\link{TrendText}}, \code{\link{plot.Trend}}
) {
	
	## init plot
	op <- par()
	if (is.null(xlim)) {
	   xlim <- range(time(x))
	   xlim[1] <- xlim[1] - 1
	}
	if (!add) plot(0, 1, xlim=xlim, ylim=c(0, 1), yaxt="n", xlab="", ylab="", ...)
	is.ens <- any("mts" == class(x))
	is.point <- length(x) == 1
	if (trend.text == 5 & !is.ens) trend.text <- 4
	
	# scale time series
	q <- quantile(x, c(0.01, 0.99), na.rm=TRUE)
	if (is.null(ylim)) ylim <- q
   df <- data.frame(y=rge, x=ylim)
   m <- lm(y~x, df)
   x2 <- coef(m)[2] * x + coef(m)[1]
   x2 <- ts(x2, start=start(x), frequency=frequency(x))
   
	# compute ensemble mean (or other function)
	if (is.ens) {
	   x.ens <- ts(apply(x, 1, ensfun, na.rm=TRUE), start=start(x), frequency=frequency(x))
	   x2.ens <- ts(apply(x2, 1, ensfun, na.rm=TRUE), start=start(x2), frequency=frequency(x2))
	}
   
   # plot time series
   lwd2 <- lwd
   if (!is.null(trend)) lwd2 <- lwd * 0.5
   if (is.ens & !is.point) {

      # plot uncertainty as polygon
      if (grepl("poly", unc)) {
         if (ncol(x2) > 3) {
            low <- apply(x2, 1, min, na.rm=TRUE)
            upp <- apply(x2, 1, max, na.rm=TRUE)
            x2[,2] <- low
            x2[,3] <- upp
         }
         col2 <- Col2(col)
         ti <- time(x2)
         polygon(c(ti, rev(ti)), c(x2[,2], rev(x2[,3])), col=col2, border=col2)
         lines(x2[,2], col=col, lwd=lwd*0.4, lty=1)
         lines(x2[,3], col=col, lwd=lwd*0.4, lty=1)
      } 
      if (grepl("line", unc)) {
         for (i in 1:ncol(x2)) lines(x2[,i], col=col, lwd=lwd*0.5, lty=3)
      }
      lines(x2.ens, col=col, lwd=lwd)
   } 
   if (!is.ens & !is.point) {
      lines(x2, col=col, lwd=lwd)
   }
   if (is.point) {
      points(x2, col=col, pch=16)
   }
   
   # add axis
   if (axis) {
      if (is.null(axis.lim)) {
         
         if (ylim[1] < q[1] | ylim[2] > q[2]) {
            yaxt <- pretty(ylim, n=7)
            yaxt[yaxt < ylim[1]] <- NA
            yaxt[yaxt > ylim[2]] <- NA
            yaxt <- na.omit(yaxt) 
            if (length(yaxt) < 2) yaxt <- pretty(ylim, n=3)
         } else {
            yaxt <- pretty(x, n=5)
         }
      } else {
         yaxt <- pretty(axis.lim, n=5)
      }
      yaxt2 <- coef(m)[2] * yaxt + coef(m)[1]
      if (axis.pos == "l") axis <- 2
      if (axis.pos == "r") axis <- 4
      par(mgp=c(1.1, 0.7, 0))
      axis(axis, yaxt2, yaxt, col=axis.col, col.ticks=axis.col, col.axis=axis.col, lwd=2, cex.axis=0.9)#, cex.axis=op$cex.axis*0.9)
      mtext(ylab, axis, 1.8, col=axis.col, at=mean(yaxt2), cex=1)#, cex=op$cex.axis*0.9)
      par(mgp=c(2.4, 1, 0))
   }
   
   # add trend?
   if (!is.null(trend) & !is.point) {
      if (is.ens) {
         trd <- TrendText(x.ens, trend=trend, period=trend.period, unit=unit)
         
         # trend for each ensemble member
         trd.df <- adply(x, 2, function(x) {
            trd <- do.call(trend, list(Yt=x, breaks=0))
            data.frame(slope=trd$slope, perc=trd$perc, mk.pval=trd$mk.pval)
         })
         wt <- wilcox.test(trd.df$slope)
         wt.p <- ""
         if (wt$p.value <= 0.05) wt.p <- "*"
         trd$ens <- trd.df

      } else {
         trd <- TrendText(x, trend=trend, period=trend.period, unit=unit)
      }

      trd1 <- coef(m)[2] * trd$trd1 + coef(m)[1]
      lty <- 2
      if (is.null(trend.period)) lty <- 1
      lines(trd1, col=col, lwd=lwd*2, lty=lty)
      if (trend.text == 1) text(mean(time(trd1)[1:4]), mean(trd1[1:4]), trd$full.perc, col=col, pos=3, cex=1.1)
      if (trend.text == 2) text(mean(time(trd1)[1:4]), mean(trd1[1:4]), trd$full.slope, col=col, pos=3, cex=1.1)
      if (trend.text == 3) text(mean(time(trd1)[1:4]), mean(trd1[1:4]), trd$full.slope2, col=col, pos=3, cex=1.1)
      if (trend.text == 4) text(time(trd1)[1], mean(trd1[1:4]), trd$full.pval, col=col, cex=2, pos=2)
      if (is.ens & trend.text == 5) text(time(trd1)[1], mean(trd1[1:4]), wt.p, col=col, cex=2, pos=2)
      
      # trend in period
      if (!is.null(trend.period)) {
         trd2 <- coef(m)[2] * trd$trd2 + coef(m)[1]
         lines(trd2, col=col, lwd=lwd*2, lty=1)
         if (trend.text == 1) text(mean(time(trd2)[1:4]), mean(trd2[1:4]), trd$subs.perc, col=col, pos=3, cex=1.1)
         if (trend.text == 2) text(mean(time(trd2)[1:4]), mean(trd2[1:4]), trd$subs.slope, col=col, pos=3, cex=1.1)
         if (trend.text == 3) text(mean(time(trd2)[1:4]), mean(trd2[1:4]), trd$subs.slope2, col=col, pos=3, cex=1.1)
         if (trend.text == 4) text(time(trd2)[1], mean(trd2[1:4]), trd$subs.pval, col=col, cex=2, pos=2)
      }
      text(time(trd1)[1], trd1[1], text, col=col, cex=1, pos=1)
   } else {
      if (is.point) text(time(x), x2, text, col=col, cex=1, pos=1)
      if (!is.point) text(time(x)[1], modal(x2[1:4]), text, col=col, cex=1, pos=2)
      trd <- NULL
   }
   return(trd)
   ### A list with information about the estimated trend.
}, ex=function() {

data(ndvi)
ndvi <- aggregate(ndvi, FUN=mean)

# default plot
MtsPlot(ndvi, ylab="NDVI")
MtsPlot(ndvi, ylab="NDVI", text="GIMMS")
MtsPlot(ndvi, rge=c(0.4, 1)) # plot time series in (0.4-1) range of y-axis
MtsPlot(ndvi, rge=c(0.5, 1), ylim=c(0,1)) # use (0-1) range for y-values and 
  # plot in (0.5-1) range of y-axis

# plot with uncertainty ranges
ndvi2 <- cbind(ndvi, ndvi * 1.3, ndvi * 0.7)
MtsPlot(ndvi2, rge=c(0.5, 1)) # uncertainty as polygon
MtsPlot(ndvi2, add=TRUE, rge=c(0, 0.5), unc="line", col="red") # uncertainty as line

# plot multiple time series
ndvi2 <- ndvi * 2 # scaled NDVI
ndvi3 <- ndvi^2 # squared NDVI
par(mar=c(3, 3, 2, 3))
MtsPlot(ndvi, rge=c(0.66, 1), ylab="NDVI")
MtsPlot(ndvi2, add=TRUE, rge=c(0.33, 0.66), axis.pos="r", ylab="NDVI*2", col="red")
MtsPlot(ndvi3, add=TRUE, rge=c(0, 0.33), axis.pos="l", ylab="NDVI^2", col="purple")

# options for trend
MtsPlot(ndvi, ylab="NDVI", trend=NULL) # no trend
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996)) # compute trend in subperiod
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=1) # text: trend in % (default)
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=2) # text: trend in units
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=3) # text: trend without unit
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=4) # text: p-value only

ndvi2 <- cbind(ndvi, ndvi * 1.3, ndvi * 0.7)
MtsPlot(ndvi2, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=5) # text: p-value only

})



