TrendSeasonalAdjusted <- structure(function(
	##title<< 
	## Trend estimation based on seasonal-adjusted time series
	
	##description<<
	## The function computes and substracts the seasonal cycle from a time series. Then a trend is estimated on the seasonal-adjusted time series. The function can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}. A detailed description of this method can be found in Forkel et al. (2013).
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	mosum.pval=0.05,
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	
	
	h=0.15, 
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.
	
	breaks=NULL, 
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.
	
	funSeasonalCycle=MeanSeasonalCycle
	### a function to estimate the seasonal cycle of the time series. A own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class "ts". Currently two approaches are part of this package:
	### \itemize{ 
	### \item{ \code{\link{MeanSeasonalCycle}} is the default which computes the average seasonal cycle from all years. }
	### \item{ \code{\link{LmSeasonalCycle}} computes the seasonal cycle based on a linear model. }
	### }
	
	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.
		
	##seealso<<
	## \code{\link{Trend}}, \code{\link{TrendRaster}}, \code{\link{MeanSeasonalCycle}}, \code{\link{LmSeasonalCycle}}
) {
	# calculate seasonal cycle
	St_est <- do.call(funSeasonalCycle, list(Yt))
	time <- time(Yt)
	
	# calculate seasonal adjusted data
	At_est <- Yt - St_est	# seasonal adjusted data

	# prepare data for analysis
	d <- bfastpp(At_est, order = 2)
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
		if (is.na(test$p.value)) test$p.value <- 9999
		if (test$p.value <= mosum.pval) calc.breaks <- TRUE
	}
		
	# estimate breakpoints based on initial trend 
	if (!no.breaks & calc.breaks) {
		bp_est <- tryCatch({
			breakpoints(response ~ trend, data = d, h=h, breaks=breaks)
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
		m <- lm(response ~ seg/trend, data = d)
	} else {
	# calculate model without breakpoint
		m <- lm(response ~ trend, data = d)
	}
	m.sum <- summary(m)

	# get trend fit line
	trend_est <- rep(NA, length(Yt))
	trend_est[d$trend] <- predict(m, d) 
	trend_est <- approx((1:length(Yt)), trend_est, xout=1:length(Yt), method="linear", rule=c(1,1))$y
	trend_est <- ts(trend_est, start=start(Yt), frequency=frequency(Yt))
	trend_est <- (trend_est - mean(trend_est, na.rm=TRUE)) + mean(Yt, na.rm=TRUE)
	
	if (!is.na(bp_est$breakpoints[1])) {
		bp_est$breakpoints <- d$trend[bp_est$breakpoints]
	}
	
	# compute MannKendall test
	mk <- MannKendallSeg(At_est, bp=bp_est)[-1,]

	# return results
	result <- list(
		series = Yt,
		adjusted = At_est,
		trend = trend_est,
		time = time,
		bp = bp_est,
		slope = mk$lm.slope, 
		slope_unc = matrix(NA, nrow=1, ncol=5),
		slope_se = mk$lm.slope.se,
		pval = mk$lm.slope.pvalue,  
		percentage = mk$lm.slope.perc,
		mk.tau = mk$mk.tau,
		mk.pval = mk$mk.pval,	
		bptest = test,
		method = "SeasonalAdjusted")

	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
},ex=function(){
# calculate trend on time series with removed mean seasonal cycle
MACtrend <- TrendSeasonalAdjusted(ndvi, funSeasonalCycle=MeanSeasonalCycle)
MACtrend
plot(MACtrend)

# plot the seasonal-adjusted time series
plot(ndvi)
lines(MACtrend$adjusted, col="orange")

# calculate trend on time series with removed mean seasonal cycle 
# but with limited number of breakpoints
MACtrend <- TrendSeasonalAdjusted(ndvi, breaks=1, funSeasonalCycle=MeanSeasonalCycle)
MACtrend
plot(MACtrend)


})
