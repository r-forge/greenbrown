AnomaliesFiltersLags <- structure(function(
	##title<< 
	## Calculate anomalies, lags and rolling windows
	
	##description<<
	## This function computes several time-variant statisttics of a time series like seasonal anomalies, time lagged versions of time series, and filters time series based on running windows (using \code{\link{rollapply}}.
	
	x,
	### univariate time series of class \code{\link{ts}}
	
	funSeasonalCycle = MeanSeasonalCycle,
	### a function to estimate the seasonal cycle of the time series.
	
	funFilter = median,
	### a function to filter the time series based on rolling windows.
	
	alignFilter = c("center", "left", "right"),
	###  specifies whether the index of the running filter results should be left- or right-aligned or centered (default) compared to the rolling window of observations. See \code{\link{rollapply}}
	
	filters = c(3, 5, 7, 9, 11, 13),
	### window sizes for rolling filters to be applied
	
	lags = -1:-7,
	### time lags to be applied for lagged time series
	
	...
	### further arguments (currently not used)
		
	##seealso<<
	## \code{\link{MeanSeasonalCycle}}
) {    
      
      # mean seasonal cycle 
      x.msc <- do.call(funSeasonalCycle, list(ts=x)) + mean(x, na.rm=TRUE) 

      # anomalies
      x.anom <- x - x.msc
      
      # filter time series 
      .Filter <- function(x, filters, funFilter) {
         x0 <- x
         for (i in 1:length(filters)) x <- ts.union(x, rollapply(x0, width=filters[i], funFilter, na.rm=TRUE, fill=NA, align=alignFilter[1]))
         colnames(x) <- c("orig", paste0("filter", filters))
         return(x)
      }      
      x.filt <- .Filter(x, filters=filters, funFilter=funFilter)
      x.anom.filt <- .Filter(x.anom, filters=filters, funFilter=funFilter)
      # plot(x.anom.filt, col=rainbow(maxFilter), plot.type="single")
      
      # time lags
      .Lags <- function(x, lags) {
         x0 <- x
         for (i in 1:length(lags)) x <- ts.union(x, lag(x0, lags[i]))
         nms <- c("orig", paste0("lag", c("neg", "pos")[(lags > 0)+1], abs(lags)))
         colnames(x) <- nms
         return(x)
      }
      x.lag <- .Lags(x, lags=lags)
      x.msc.lag <- .Lags(x.msc, lags=lags)
      x.anom.lag <- .Lags(x.anom, lags=lags)
      
      # results
      res <- ts.union(x, x.msc, x.anom, x.filt, x.anom.filt, x.lag, x.msc.lag, x.anom.lag)
      colnames(res) <- c("orig", "msc", "anom", 
         paste0("orig.", colnames(x.anom.filt)), 
         paste0("anom.", colnames(x.anom.filt)), 
         paste0("orig.", colnames(x.lag)), 
         paste0("msc.", colnames(x.msc.lag)), 
         paste0("anom.", colnames(x.anom.lag)))
      res <- res[, -(grep(".orig", colnames(res)))]
      return(res)
      ### The function returns a multivariate time series of class 'mts' with the following columns:
      ### \itemize{ 
	   ### \item{ \code{orig} the original time series }
	   ### \item{ \code{msc} mean seasonal cycle as computed with \code{funSeasonalCycle} (repeated for the full time series length) }
	   ### \item{ \code{anom} anomalies releative to mean seasonal cycle }
	   ### \item{ \code{orig.filterX} rolling window result based on the original time series as computed with \code{funFilter} for the filter window size X }
	   ### \item{ \code{anom.filterX} rolling window result based on the anomaly time series as computed with \code{funFilter} for the filter window size X }
	   ### \item{ \code{orig.lagX} time lagged version of the original time series for the time lag X }	
	   ### \item{ \code{msc.lagX} time lagged version of the mean seasonal cycle time series for the time lag X }		 
	   ### \item{ \code{anom.lagX} time lagged version of the anomaly time series for the time lag X }     
	### }
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# do calculations
afl <- AnomaliesFiltersLags(ndvi)
colnames(afl)

# seasonal anomalies
plot(afl[,"anom"]) 

# running median filters on original time series
plot(afl[, grep("orig.filter", colnames(afl))], plot.type="single", col=rainbow(6)) 

# running median filters on anomalies
plot(afl[, grep("anom.filter", colnames(afl))], plot.type="single", col=rainbow(6)) 

# lagged versions of original time series
plot(window(afl[, grep("orig.lag", colnames(afl))], start=c(1995, 1), end=c(2000, 12)), plot.type="single", col=rainbow(7), type="l") 

# lagged versions of anomaly time series
plot(afl[, grep("anom.lag", colnames(afl))], plot.type="single", col=rainbow(7)) 

})  
      

      

