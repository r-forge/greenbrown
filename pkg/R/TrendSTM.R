TrendSTM <- structure(function(	
	##title<< 
	## Trend estimation based on a season-trend model
		
	##description<<
	## The trend and breakpoint estimation in method STM is based on the classical additive decomposition model and is following the implementation as in the bfast approach (\url{http://cran.r-project.org/web/packages/bfast/index.html}) (Verbesselt et al. 2010, 2012). Linear and harmonic terms are fitted to the original time series using ordinary least squares regression. This method can be also used to detect breakpoints in the seasonal component of a time series. The function can be applied to gridded (raster) data using the function \code{\link{TrendRaster}}.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	h=0.15,
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. 
	
	breaks=NULL, 
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used.
	
	mosum.pval=0.05
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.
	
	##references<<
	## Verbesselt, J.; Hyndman, R.; Zeileis, A.; Culvenor, D., Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment 2010, 114, 2970-2980. \cr
	## Verbesselt, J.; Zeileis, A.; Herold, M., Near real-time disturbance detection using satellite image time series. Remote Sensing of Environment 2012, 123, 98-108.
 
	##seealso<<
	## \code{\link{Trend}}, \code{\link{TrendRaster}}
) {

	require(strucchange)
	
	# prepare data for analysis
	d <- bfastpp(Yt, order = 2)
	
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
		test <- sctest(response ~ trend + harmon, data=d, type="OLS-MOSUM", h=h)
		if (test$p.value <= mosum.pval) calc.breaks <- TRUE
	}
	
	# estimate breakpoints based on initial trend and harmonics
	if (!no.breaks & calc.breaks) {
		bp_est <- tryCatch({
			breakpoints(response ~ trend + harmon, data = d, h=h, breaks=breaks)
		}, warning = function(w) {
			NoBP()
		}, error = function(e) {
			NoBP()
		}, finally = function(x) {
			NoBP()
		})
		bpseasonal <- NULL
	} else {
		bp_est <- NoBP()
		bpseasonal <- NULL
	}
	
	# calculate models with breakpoints
	if (!is.na(bp_est$breakpoints[1])) {
		d$seg <- breakfactor(bp_est)
		
		# calculate regression with breakpoints affecting ...
		m1 <- lm(response ~ seg/trend, data = d)	# ... trend
		m2 <- lm(response ~ seg/trend + harmon, data = d) # ... trend
		m3 <- lm(response ~ seg/(trend + harmon), data = d)	# ... trend and harmonics
		m4 <- lm(response ~ trend + seg/harmon, data = d)	# ... harmonics only
		
		# select best regression model
		bic <- BIC(m1, m2, m3, m4) 
		bic.best <- which.min(bic$BIC)
		
		# select only model where breaks are in trend component
		if (bic.best == 1) {
			m <- m1
			bptype <- "trend breakpoint"
		}
		if (bic.best == 2) {
			m <- m2
			bptype <- "trend breakpoint"
		}
		# if breakpoints are in seasonal component: calculate trend model without breakpoints
		if (bic.best == 3) {
			m <- m3
			bptype <- "trend and seasonal breakpoint"
		}
		if (bic.best == 4) {
			m <- lm(response ~ trend + harmon, data = d)
			bptype <- "seasonal breakpoint"
			bp_est <- NoBP()
			bpseasonal <- bp_est
		}
	} else {
	# calculate model without breakpoint
		m <- lm(response ~ trend + harmon, data = d)
		bptype <- ""
	}
	m.sum <- summary(m)

	# get trend fit line
	d$harmon <- d$harmon * 0
	trend_est <- rep(NA, length(Yt))
	trend_est[d$trend] <- predict(m, d) 
	trend_est <- approx((1:length(Yt)), trend_est, xout=1:length(Yt), method="linear", rule=c(1,1))$y
	trend_est <- ts(trend_est, start=start(Yt), frequency=frequency(Yt))
	trend_est <- (trend_est - mean(trend_est, na.rm=TRUE)) + mean(Yt, na.rm=TRUE)

	# results: trend and pvalue as significance from regression coefficient
	pval_est <- m.sum$coefficients[grep("trend", rownames(m.sum$coefficients)),4]
	slope_est <- m.sum$coefficients[grep("trend", rownames(m.sum$coefficients)),1]	
	
	if (!is.na(bp_est$breakpoints[1])) {
		bp_est$breakpoints <- d$trend[bp_est$breakpoints]
	}

	# return results
	result <- list(
		series = Yt,
		trend = trend_est,
		time = as.vector(time(Yt)),
		bp = bp_est,
		slope = slope_est,
		pval = pval_est,
		bptest = test,
		bptype = bptype,
		bpseasonal = bpseasonal,
		method = "STM")
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
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend 
trd <- TrendSTM(ndvi)
trd
plot(trd)


})
