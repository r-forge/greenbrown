Trend <- structure(function(
	##title<< 
	## Calculate trends and trend changes in time series
	##description<<
	## This function calculates trends and trend changes (breakpoints) in a time series. It is a common interface to the functions \code{\link{TrendAAT}}, \code{\link{TrendSTM}} and \code{\link{TrendSeasonalAdjusted}}. With \code{\link{TrendRaster}} all trend analysis functions can be applied to gridded (raster) data. A detailed description of these methods can be found in Forkel et al. (2013).
	
	Yt, 
	### univariate time series of class \code{\link{ts}}
	
	method=c("AAT", "STM", "SeasonalAdjusted"), 
	### method to be used for trend calculation with the following options: 
	### \itemize{
	### \item{ \code{AAT} (default) calculates trends on annual aggregated time series (see \code{\link{TrendAAT}} for details). This method will be automatically choosen if the time series has a frequency of 1 (e.g. in case of annual time steps). If the time series has a frequency > 1, the time series will be aggregated to annual time steps using the mean. }
	### \item{ \code{STM} fits harmonics to the seasonal time series to model the seasonal cycle and to calculate trends based on a multiple linear regression (see \code{\link{TrendSTM}} for details). }
	### \item{ \code{SeasonalAdjusted} removes first the seasonal cycle from the time series and calculates the trend on the reaminder series (see \code{\link{TrendSeasonalAdjusted}} for details). }
	### }
	
	mosum.pval=0.05,
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	
	
	h=0.15, 
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.
	
	breaks=NULL, 
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.
	
	funSeasonalCycle=MeanSeasonalCycle,
	### a function to estimate the seasonal cycle of the time series if \code{SeasonalAdjusted} is selected as method. A own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class \code{\link{ts}}. Currently two approaches are part of this package:
	### \itemize{ 
	### \item{ \code{\link{MeanSeasonalCycle}} is the default which computes the mean seasonal cycle. }
	### \item{ \code{\link{SSASeasonalCycle}} detects a modulated seasonal cycle based on Singular Spectrum Analysis. }
	### }
	
	funAnnual=mean
	### function to aggregate time series to annual values if \code{AAT} is selected as method. The default function is the mean (i.e. trend calculated on mean annual time series). See \code{\link{TrendAAT}} for other examples
	
	##details<<
	## This function allows to calculate trends and trend changes based on different methods: see \code{\link{TrendAAT}}, \code{\link{TrendSTM}} or \code{\link{TrendSeasonalAdjusted}} for more details on these methods.
	## These methods can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}.
	
	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.
	
	##seealso<<
	## \code{\link{plot.Trend}}, \code{\link{TrendAAT}}, \code{\link{TrendSTM}}, \code{\link{TrendSeasonalAdjusted}}, \code{\link{TrendRaster}}, \code{\link{breakpoints}}

) {
	if (class(Yt) != "ts") stop("Yt should be from class 'ts'.")
	
	# breakpoints should be calculated?	
	sum.na <- sum(is.na(Yt))
	no.breaks <- FALSE
	if (!is.null(breaks)) {
		if (breaks == 0) no.breaks <- TRUE	# calculate no breakpoints if breaks == 0
	}
	
	# return empty Trend object if less than 3 points are not NA
	if (sum(!is.na(Yt)) < 3) {
		Yt.na <- Yt
		Yt.na[!is.na(Yt)] <- NA
		bp_est <- NULL
		bp_est$breakpoints <- NA
		trend <- list(
			series = Yt,
			trend = Yt.na,
			time = as.vector(time(Yt)),
			bp = bp_est,
			slope = NA,
			bptest=NA,
			pval = NA)
		class(trend) <- "Trend"
		warning("Yt has less than 4 non-NA values. Trend was not calculated.")
		return(trend)
	}
	
	# compute trend
	if (frequency(Yt) <= 1) method <- "AAT"
	if (method[1] == "AAT") trend <- TrendAAT(Yt, h=h, breaks=breaks, mosum.pval=mosum.pval, funAnnual=funAnnual)
	if (method[1] == "STM") trend <- TrendSTM(Yt, h=h, breaks=breaks, mosum.pval=mosum.pval)
	if (method[1] == "SeasonalAdjusted") trend <- TrendSeasonalAdjusted(Yt, h=h, breaks=breaks, funSeasonalCycle=funSeasonalCycle, mosum.pval=mosum.pval)
	return(trend)
	### The function returns a list of class "Trend" with the following components:
	### \itemize{ 
	### \item{ \code{series} time series on which the trend was calculated. }
	### \item{ \code{trend} time series with the estimated trend. }
	### \item{ \code{time} a vector of time steps. }
	### \item{ \code{bp} an object of class \code{"breakpoints"}. See \code{\link{breakpoints}} for details. }
	### \item{ \code{slope} a vector of the trend slopes for each trend segment. }
	### \item{ \code{pval} a vector of the p-values of teh trend for each trend segment. }
	### }
},ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend (default method: trend calculated based on annual aggregated data)
trd <- Trend(ndvi)
trd
plot(trd)

# an important parameter is mosum.pval, if the p-value is changed to 1 breakpoints can be detected in the time series regardless if there is significant structural change
trd <- Trend(ndvi, mosum.pval=1)
trd
plot(trd)

# calculate trend based on modelling the seasonal cycle
trd <- Trend(ndvi, method="STM")
trd
plot(trd)

# calculate trend based on removal of the seasonal cycle
trd <- Trend(ndvi, method="SeasonalAdjusted", funSeasonalCycle=SSASeasonalCycle)
plot(trd)
trd

# calculate trend based on removal of the seasonal cycle
trd <- Trend(ndvi, method="SeasonalAdjusted", funSeasonalCycle=MeanSeasonalCycle)
plot(trd)
lines(trd$adjusted, col="green")
trd

# calculate trend based on removal of the seasonal cycle: modify maximal number of breakpoints
trd <- Trend(ndvi, method="SeasonalAdjusted", breaks=1, funSeasonalCycle=MeanSeasonalCycle)
plot(trd)
trd

# calculate trend based on removal of the seasonal cycle: modify minimum segment size, e.g. set the minimum length of segments to 8 years
trd <- Trend(ndvi, method="SeasonalAdjusted", h=(8*12)/length(ndvi))
plot(trd)
trd

})

