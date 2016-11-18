plot.PhenopixMY <- structure(function(
	##title<< 
	## Plot multi-year phenopix objects
	##description<<
	## Plotting methods for objects of class \code{\link{PhenopixMY}}
	
	x,
	### an object of class \code{\link{PhenopixMY}}
		
	add = FALSE,
	### add to existing plot?
	
	col.fit = "black",
	### color for fitting line
	
	type = "ts",
   ### plot type: 'ts' plots the original data, the fitted curve and the metrics; 'metrics' plots only time series of the metrics

	...
	### further arguments as in \code{\link{plot.default}}
	
	##references<< 
	## Filippa, G., Cremonese, E., Migliavacca, M., Galvagno, M., Forkel, M., Wingate, L., Tomelleri, E., Morra di Cella, U. and Richardson, A. D.: Phenopix: A R package for image-based vegetation phenology, Agricultural and Forest Meteorology, 220, 141-150, doi:10.1016/j.agrformet.2016.01.006, 2016.
		
	##seealso<<
	## \code{\link{plot.phenopix}}, \code{\link{PhenopixMY}}
) { 

   if (type == "metrics") {
      if (!is.null(x$metrics)) {
         bool <- apply(x$metrics, 2, function(x) all(is.na(x)))
         metrics <- x$metrics[, !bool]
         plot(metrics)
      } else {
         type <- "ts"
         warning("x has no metrics. Plotting default plot type.")
      }
   } 
   
   if (type != "metrics") {

      if (!add) plot(x$data, col="darkgrey", ...)
      lines(x$fit, col=col.fit, lwd=2)
      
      if (!is.null(x$metrics)) {
         for (year in x$years) {
            metrics <- x$metrics[year == time(x$metrics)]
            
            if (length(metrics) != 10) {
               the.seq <- c(1:4)
            } else {
               the.seq <- c(1, 2, 4)
            }
            
            if (year == x$years[1]) {
            fit <- x$ppix[[grep(year, x$years)]]$fit$fit$predict
            ypos <- seq(min(fit), max(fit), length=length(the.seq))
            }
            
            cols <- palette()[1:length(the.seq)+1]
            metrics2 <- as.Date(paste(year, metrics[, the.seq]), "%Y %j")
            
            abline(v=metrics2, col=cols, lty=3)
            txt <- paste(names(metrics)[the.seq], "=", signif(metrics[1,the.seq], 2))
            text(metrics2, y=ypos, labels = txt, col = cols, srt=45)
         } # end loop over years
      } # end if metrics
   } # end else type
}, ex=function() {
data(ndvi)
plot(ndvi)

ppixmy <- PhenopixMY(ndvi, "spline", "trs")
plot(ppixmy)

plot(ppixmy, type="metrics")

})
