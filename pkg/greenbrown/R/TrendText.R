TrendText <- structure(function(
	##title<< 
	## Text of trends in time series
	
	##description<<
	## The function computes a trend in a time series and creates text strings of the estimated trend that ca be used in plots.
	
	Yt,
	### a time series
	
	trend = TrendAAT,
	### a function how the trend should be computed, e.g. \code{\link{TrendAAT}}, \code{\link{TrendRQ}}, \code{\link{TrendSTM}}
	
	period = NULL,
	### an additional sub-period of x for which trends should be additionally computed
	
	unit = "unit",
	### unit of the values in Yt
	
	per.year = FALSE
	### Is unit per year? If TRUE, trend is in % yr-2 !
	
	##details<< 
	## This function is used in \code{\link{MtsPlot}}
	
	##seealso<< 
	## \code{\link{MtsPlot}}
) {	

   if (is.null(period)) period <- c(start(Yt)[1], end(Yt)[1]+1)
   Yt2 <- window(Yt, start=period[1], end=period[2])
   
   # trend for full time series
   #---------------------------
   
   trd1 <- do.call(trend, list(Yt=Yt))
   unc1 <- TrendUncertainty(Yt, trend=trend, sample.min.length = 0.7)
   yr <- 1
   if (per.year) yr <- 2
   vals <- list(NULL)
   
   sl <- signif(unc1[[1]]$perc, 2)
   med <- signif(unc1[[1]]$perc_unc[3], 2)
   up <- signif(unc1[[1]]$perc_unc[2], 2)
   low <- signif(unc1[[1]]$perc_unc[1], 2)
   pval1 <- round(unc1[[1]]$mk.pval, 2)
   pval1.sym <- ""
   if (pval1 <= 0.05) pval1.sym <- "*"
   full.perc <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~"%"~yr^-.(yr)~.(pval1.sym)) 
   full.perc2 <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(pval1.sym)) 
   vals$full.perc.sl <- sl
   vals$full.perc.med <- med
   vals$full.perc.low <- low
   vals$full.perc.upp <- up
   vals$full.pval <- pval1
   
   sl <- signif(unc1[[1]]$slope, 1)
   med <- signif(unc1[[1]]$slope_unc[3], 1)
   up <- signif(unc1[[1]]$slope_unc[2], 1)
   low <- signif(unc1[[1]]$slope_unc[1], 1)
   full.slope <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(unit)~yr^-.(yr)~.(pval1.sym)) 
   full.slope2 <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(pval1.sym)) 
   vals$full.slope.sl <- sl
   vals$full.slope.med <- med
   vals$full.slope.low <- low
   vals$full.slope.upp <- up
   
   
   # trend for temporal subset
   #--------------------------
   
   trd2 <- do.call(trend, list(Yt=Yt2))
   unc2 <- TrendUncertainty(Yt2, trend=trend, sample.min.length = 0.7)
   
   sl <- signif(unc2[[1]]$perc, 2)
   med <- signif(unc2[[1]]$perc_unc[3], 2)
   up <- signif(unc2[[1]]$perc_unc[2], 2)
   low <- signif(unc2[[1]]$perc_unc[1], 2)
   pval2 <- signif(unc2[[1]]$mk.pval, 2)
   pval2.sym <- ""
   if (pval2 <= 0.05) pval2.sym <- "*"
   subs.perc <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~"%"~yr^-.(yr)~.(pval2.sym)) 
   subs.perc2 <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(pval2.sym)) 
   vals$subs.perc.sl <- sl
   vals$subs.perc.med <- med
   vals$subs.perc.low <- low
   vals$subs.perc.upp <- up
   vals$subs.pval <- pval1
   
   sl <- signif(unc2[[1]]$slope, 1)
   med <- signif(unc2[[1]]$slope_unc[3], 1)
   up <- signif(unc2[[1]]$slope_unc[2], 1)
   low <- signif(unc2[[1]]$slope_unc[1], 1)
   subs.slope <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(unit)~yr^-.(yr)~.(pval2.sym)) 
   subs.slope2 <- bquote(.(sl)~scriptstyle(atop(.(up), .(low)))~.(pval2.sym)) 
   vals$subs.slope.sl <- sl
   vals$subs.slope.med <- med
   vals$subs.slope.low <- low
   vals$subs.slope.upp <- up
   
   l <- list(ts=Yt, trd1=trd1$trend, trd2=trd2$trend, full.perc=full.perc, full.perc2=full.perc2, full.slope=full.slope, full.slope2=full.slope2, subs.perc=subs.perc, subs.perc2=subs.perc2, subs.slope=subs.slope, subs.slope2=subs.slope2, full.pval=pval1.sym, subs.pval=pval2.sym, values=vals)
   return(l)
   ### A list with the estimated trends and with text.
}, ex=function() {

data(ndvi)
trd <- TrendText(ndvi, period=c(1982, 1996), unit="")
plot(ndvi)
lines(trd$trd1, col="red", lwd=2)
lines(trd$trd2, col="blue", lwd=2, lty=2)
text(2003, 0.19, trd$full.perc, col="red", cex=1.5) # full trend in %
text(2003, 0.36, trd$full.slope, col="red", cex=1.5) # full trend slope with unit
text(2003, 0.34, trd$full.slope2, col="red", cex=1.5) # full trend slope without unit
text(1988, 0.19, trd$subs.perc, col="blue", cex=1.5) # period: trend in %
text(1988, 0.36, trd$subs.slope, col="blue", cex=1.5) # period: trend slope with unit
text(1988, 0.34, trd$subs.slope2, col="blue", cex=1.5) # period: trend slope without unit

})
