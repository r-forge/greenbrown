TrendAAT <- structure(function(
	##title<< 
	## Trend estimation based on annual aggregated time series
	
	##description<<
	## The function aggregates a time series to annual values and computes breakpoints and trends on the annual aggregated time series. The function can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}. A detailed description of this method can be found in Forkel et al. (2013).
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	mosum.pval=0.05,
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	
	
	h=0.15, 
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.
	
	breaks=NULL, 
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.
	
	funAnnual=mean
	### function to aggregate time series to annual values The default function is the mean (i.e. trend calculated on mean annual time series). See example section for other examples.
	
	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.
		
	##seealso<<
	## \code{\link{Trend}}, \code{\link{TrendRaster}}
) {
	time <- time(Yt)	
	
	# aggregate series to annual time steps
	if (frequency(Yt) > 1) {
		years <- as.integer(as.vector(time))
		nyears <- length(unique(years))
		Yt <- aggregate(as.vector(Yt), list(years), FUN=funAnnual, na.rm=TRUE)$x
		Yt <- ts(Yt, start=years[1], end=years[length(years)], freq=1)
	}
	time <- time(Yt)	
	d <- bfastpp(Yt)
	if (nrow(d) < 2 | AllEqual(d$response)) return(NoTrend(Yt))
	
	# breakpoints should be calculated?	
	sum.na <- sum(is.na(Yt))
	no.breaks <- FALSE
	if (!is.null(breaks)) {
		if (breaks == 0) no.breaks <- TRUE	# calculate no breakpoints if breaks == 0
	}
	
	# test for breakpoints
	calc.breaks <- FALSE
	test <- NULL
	if (!no.breaks) {	
		test <- sctest(response ~ trend, data=d, type="OLS-MOSUM", h=h)
		if (test$p.value <= mosum.pval) calc.breaks <- TRUE
	}
		
	# estimate breakpoints
	if (!no.breaks & calc.breaks) {
		bp_est <- tryCatch({
			breakpoints(response ~ trend, h=h, breaks=breaks, data=d)
		}, warning = function(w) {
			NoBP()
		}, error = function(e) {
			NoBP()
		}, finally = function(x) {
			NoBP()
		})

	} else {
		bp_est <- NoBP()
	}
	
	# calculate models with breakpoints
	if (!is.na(bp_est$breakpoints[1])) {
		d$seg <- breakfactor(bp_est)
		m <- lm(response ~ seg / trend, data = d)
	} else {
	# calculate model without breakpoint
		m <- lm(response ~ trend, data = d)
	}
	m.sum <- summary(m)

	# estimate trend component
	trend_est <- rep(NA, length(Yt))
	trend_est[d$trend] <- predict(m, d) 
	trend_est <- approx((1:length(Yt)), trend_est, xout=1:length(Yt), method="linear", rule=c(1,1))$y
	trend_est <- ts(trend_est, start=start(Yt), frequency=frequency(Yt))
	trend_est <- (trend_est - mean(trend_est, na.rm=TRUE)) + mean(Yt, na.rm=TRUE)
	
	# results: pvalue with MannKendall test
	if (!is.na(bp_est$breakpoints[1])) { 
		pval_est <- aggregate(na.omit(as.vector(Yt)), by=list(d$seg), FUN=function(x) MannKendall(x)$sl )$x
	} else {
		pval_est <- MannKendall(Yt)$sl 
	}
	slope_est <- m.sum$coefficients[grep("trend", rownames(m.sum$coefficients)),1]
	
	if (!is.na(bp_est$breakpoints[1])) {
		bp_est$breakpoints <- d$trend[bp_est$breakpoints]
	}

	# return results
	result <- list(
		series = Yt,
		trend = trend_est,
		time = as.vector(time),
		bp = bp_est,
		slope = slope_est,
		pval = pval_est,
		bptest = test,
		method = "AAT")
	class(result) <- "Trend"
	return(result)
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
	
# calculate trend on mean annual NDVI values
trd.annualmean <- TrendAAT(ndvi)
trd.annualmean
plot(trd.annualmean)

# calculate annual trend but don't apply MOSUM test for structural change
trd.annualmean <- TrendAAT(ndvi, mosum.pval=1)
trd.annualmean
plot(trd.annualmean)

# calculate trend on annual peak (maximum) NDVI
trd.annualmax <- TrendAAT(ndvi, funAnnual=max, mosum.pval=1)
trd.annualmax 
plot(trd.annualmax )

# calculate trend on an annual quantile NDVI (e.g. upper 0.9 quantile)
fun <- function(x, ...) { quantile(x, 0.9, ...) }
trd.annualquantile9 <- TrendAAT(ndvi, funAnnual=fun, mosum.pval=1)
trd.annualquantile9
plot(trd.annualquantile9)

# calculate trend on an winter NDVI (e.g. upper 0.1 quantile)
fun <- function(x, ...) { quantile(x, 0.1, ...) }
trd.annualquantile1 <- TrendAAT(ndvi, funAnnual=fun, mosum.pval=1)
trd.annualquantile1
plot(trd.annualquantile1)

# compare trends
plot(ndvi)
lines(trd.annualmean$trend, col="green")
lines(trd.annualmax$trend, col="red")
lines(trd.annualquantile9$trend, col="orange")
lines(trd.annualquantile1$trend, col="blue")
})

