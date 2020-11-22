Decompose <-  structure(function(
	##title<< 
	## Simple decomposition of time series
	
	##description<<This function decomposes time series in different components using a simple step-wise approach.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	breaks=0,
	### maximal number of breaks in the trend component to be calculated (integer number). 
	
	mosum.pval=0.05
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not.
	
	##details<<
	## The decomposition of the time series is based on a simple step-wise approach:
	## \itemize{
	## \item{ The mean of the NDVI time series is calculated. }
	## \item{ In the second step, monthly values are aggregated per year by using the average value and the trend is calculated based on annual aggregated values using \code{ \link{TrendAAT}}. }
	## \item{ The mean of the time series and the derived trend component from step (2) are subtracted from the annual values to derive the trend-removed and mean-centred annual values (annual anomalies). If the trend slope is not significant (p > 0.05), only the mean is subtracted. }
	## \item{ In the next step, the mean, the trend component and the annual anomalies are subtracted from the original time series to calculate a detrended, mean-centered and for annual anomalies adjusted time series. From this time series the seasonal cycle is estimated as the mean seasonal cycle. }
	## \item{ In the last step, the short-term anomalies are computed. For this, the mean, the trend component, the annual anomalies and the mean seasonal cycle are subtracted from the original time series. } }
	
	##references<<
	## Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.
	
	##seealso<<
	## \code{\link{GetTsStatisticsRaster}} 	
) {
	
  if (class(Yt) != "ts") 
    stop("ts should be of class ts.")
  time <- time(Yt)
  start <- start(Yt)
  seas <- cycle(Yt)
  freq <- frequency(Yt)
  years <- as.integer(as.vector(time))
  nyears <- length(unique(years))
  obs.per.year <- table(years)
  n <- length(Yt)
  has.cycle <- (length(unique(seas)) > 1)
  
  if (sum(!is.na(Yt)) < 4) {
    x <- Yt
    x[] <- NA
    components.ts <- cbind(Mean = x, Trend = x, 
                           IAV = x, Seasonal = x, 
                           Anomaly = x)
  } else {
    
    if (has.cycle) {
      annual.oneval.ts <- ts(aggregate(as.vector(Yt), by = list(years), 
                                       FUN = "mean", na.rm = TRUE)$x, start = start, frequency = 1)
      annual.ts <- annual.oneval.ts[rep(1:nyears, obs.per.year)]
    }
    else {
      annual.ts <- annual.oneval.ts <- Yt
    }
    annual.mean.ts <- mean(annual.ts, na.rm = TRUE)
    trend.trd <- Trend(annual.oneval.ts, breaks = breaks, method = "AAT", 
                       h = 0.15, mosum.pval = mosum.pval)
    trend.ts <- ts(trend.trd$trend[rep(1:nyears, obs.per.year)], 
                   start = start, frequency = freq)
    if (any(trend.trd$pval <= 0.05)) {
      annual.anomaly.ts <- annual.ts - trend.ts
      trend.ts <- trend.ts - annual.mean.ts
    }
    else {
      trend.ts <- ts(rep(0, n), start = start, frequency = freq)
      annual.anomaly.ts <- annual.ts - annual.mean.ts
    }
    if (has.cycle) {
      seasonal.ts <- Yt - annual.mean.ts - trend.ts - annual.anomaly.ts
      mean.seasonal.ts <- MeanSeasonalCycle(seasonal.ts)
      seasonal.anomaly.ts <- Yt - annual.mean.ts - trend.ts - 
        annual.anomaly.ts - mean.seasonal.ts
    }
    else {
      mean.seasonal.ts <- ts(rep(0, n), start = start, frequency = freq)
      seasonal.anomaly.ts <- ts(rep(0, n), start = start, frequency = freq)
    }
    components.ts <- cbind(Mean = annual.mean.ts, Trend = trend.ts, 
                           IAV = annual.anomaly.ts, Seasonal = mean.seasonal.ts, 
                           Anomaly = seasonal.anomaly.ts)
  }
  return(components.ts)
	### The function returns a multi-variate object of class ts including the time series components.
}, ex=function() {

# decompose a time series
ndvi.dc <- Decompose(ndvi)
plot(ndvi.dc)
	
ndvi.dc2 <- Decompose(ndvi, breaks=2, mosum.pval=1)
plot(ndvi.dc2)

})





