PhenopixMY <- structure(function(
	##title<< 
	## Multi-year phenology analysis using phenopix
	##description<<
	## This function takes a multi-year time series and applies curve fitting and phenology extraction functions based on the \code{\link{greenProcess}} function in the \code{phenopix} package. The function returns an object of class \code{PhenopixMY} (phenopix multi-year) which contains a list of \code{phenopix} objects. \code{PhenopixMY} can be plotted using \code{\link{plot.PhenopixMY}}.
	
	ts,
	### a time series of class 'ts' or 'zoo' with multiple years of data
	
	fit,
	### fitting function to be applied, available options are: spline, beck, elmore, klosterman, gu (see \code{\link{greenProcess}})
	
	threshold=NULL,
	### threshold to be applied to compute phenology metrics, available options are: trs, derivatives, klosterman, gu (see \code{\link{greenProcess}}) 

	plot = FALSE,
	### plot phenopix object of each year, using \code{\link{plot.phenopix}}
	
	...
	### further arguments as in \code{\link{greenProcess}}
	
	##references<< 
	## Filippa, G., Cremonese, E., Migliavacca, M., Galvagno, M., Forkel, M., Wingate, L., Tomelleri, E., Morra di Cella, U. and Richardson, A. D.: Phenopix: A R package for image-based vegetation phenology, Agricultural and Forest Meteorology, 220, 141-150, doi:10.1016/j.agrformet.2016.01.006, 2016.
		
	##seealso<<
	## \code{\link{greenProcess}}, \code{\link{plot.PhenopixMY}}, \code{\link{Phenology}} 

) { 

   if (class(ts) == "ts") {
      # convert 'ts' to 'zoo'
      years <- floor(time(ts))
      fyear <- cycle(ts) / frequency(ts)
      doy <- fyear * 365 
      ti <- as.Date(paste(years, doy), "%Y %j")
      ts <- na.omit(zoo(ts, ti))
   } else if (class(ts) == "zoo") {
      
   } else {
      stop(paste("Class", class(ts), "is not supported from greenProcessMY."))
   }
   years <- format(time(ts), "%Y")
   years0 <- unique(years)
   
   # iterate over years: apply greenProcess
   ppix.l <- llply(as.list(years0), function(year, fit, threshold, plot) {  #, ...) {
      ts.y <- ts[year == years]
      ppix <- try(greenProcess(ts.y, fit=fit, threshold=threshold, plot=plot))   #, ...), silent=TRUE)
      if (class(ppix) == "try-error") {
         ppix <- NULL
      } else {
         # predict full time series
         if (!is.null(ppix$fit$fit$formula)) {
            l <- as.list(ppix$fit$fit$params)
            pars <- names(ppix$fit$fit$params)
            l[[length(l)+1]] <- 1:365
            names(l) <- c(pars, "t")
            intp <- eval(ppix$fit$fit$formula, envir=l)
            ppix$fit$fit$predicted <- zoo(intp, l[[length(l)]])
         } else {
            pred <- ppix$fit$fit$predicted
            time(pred) <- as.Date(paste(year, time(pred)), "%Y %j")
            intp <- AllTsteps(pred, by="day", exclude.feb29=TRUE, start.jan=TRUE, end.dec=TRUE)
            intp <- na.spline(intp)
            time(intp) <- format(time(intp), "%j")
            ppix$fit$fit$predicted <- intp
         }
      }

      return(ppix)
   }, fit=fit, threshold=threshold, plot=plot)  #, ...)
   names(ppix.l) <- years0
   
   # merge fitted phenopix time series 
   pred <- AllTsteps(ts, by="day", exclude.feb29=TRUE, start.jan=TRUE, end.dec=TRUE)
   pred[] <- NA
   years <- format(time(pred), "%Y")
   for (year in years0) {
      ppix <- ppix.l[[grep(year, years0)]]
      if (!is.null(ppix)) pred[year == years] <- ppix$fit$fit$predicted
   }
   
   # remove outliers in filtered series
   mn <- min(ts, na.rm=TRUE)
	mx <- max(ts, na.rm=TRUE)
   pred[pred < mn] <- mn
   pred[pred > mx] <- mx
   
   # create time series of metrics
   if (!is.null(threshold)) {
      metrics <- laply(ppix.l, function(ppix) {
         if (!is.null(ppix)) return(ppix$metrics)
         if (is.null(ppix)) return(NA)
      })
      metrics <- apply(metrics, 2, CorrectDOY)
      metrics <- zoo(metrics, years0)
   } else {
      metrics <- NULL
   }
   
   # phenopixmy object
   ppixmy <- list(data=ts, fit=pred, years=years0, ppix=ppix.l, metrics=metrics)
   class(ppixmy) <- "PhenopixMY"
   return(ppixmy)
   ### An object of class \code{phenopixmy} with dedicated functions: plot(), print(). The structure is actually a list.
}, ex=function() {

data(ndvi)
plot(ndvi)

ppixmy <- PhenopixMY(ndvi, "spline", "trs")
plot(ppixmy)

plot(ppixmy, type="metrics")

})








