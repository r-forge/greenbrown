TrendRQ <- structure(function(
	##title<< 
	## Trend estimation based on quantile regression
	
	##description<<
	## The function computes breakpoints and trends based on quantile regression.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
   tau = 0.5,
   ### quantile to be estimated in quantile regression
   
   ...
   ### additional arguments (currently not used)
	
	##references<< 
		
	##seealso<<
	## \code{\link{Trend}}, \code{\link{TrendAAT}}
) {
	time <- time(Yt)	
	
	d <- data.frame(response = Yt, trend=1:length(Yt))
	m <- rq(response ~ trend, data = d, tau=tau)
	m.sum <- summary(m)

	# estimate trend component
	trend_est <- rep(NA, length(Yt))
	trend_est[d$trend] <- predict(m, d) 
	trend_est <- approx((1:length(Yt)), trend_est, xout=1:length(Yt), method="linear", rule=c(1,1))$y
	trend_est <- ts(trend_est, start=start(Yt), frequency=frequency(Yt))
	trend_est <- (trend_est - mean(trend_est, na.rm=TRUE)) + mean(Yt, na.rm=TRUE)
	
	# compute MannKendall test
	mk <- MannKendallSeg(Yt)[-1,]
	
	# get results
	slope <- coef(m)[2]
	perc <- abs(slope) / abs(mean(Yt, na.rm=TRUE)) * 100
	if (slope < 0) perc <- perc * -1
	slope_unc <- data.frame(1, coef(m.sum)[2,2], coef(m.sum)[2,3], slope)
	colnames(slope_unc) <- c(".id", colnames(coef(m.sum))[2:3], "avg")
	perc_unc <- abs(slope_unc) / abs(mean(Yt, na.rm=TRUE)) * 100
	perc_unc[slope_unc < 0] <- perc_unc[slope_unc < 0] * -1
	
	# return results
	result <- list(
		series = Yt,
		trend = trend_est,
		time = as.vector(time),
		bp = NoBP(),
      slope = slope, 
		slope_unc = slope_unc,
		slope_se = NA,
		pval = NA,  
		perc = perc,
		perc_unc = perc_unc,
		mk.tau = mk$mk.tau,
		mk.tau_unc = NoUnc(),
		mk.pval = mk$mk.pval,
		bptest = NULL,
		method = paste("RQ with quantile =", tau))
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
},ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
ndvi <- window(aggregate(ndvi, FUN=mean), end=1996)
plot(ndvi)
	
# calculate trend based on regression to median
trd.q05 <- TrendRQ(ndvi)
trd.q05
plot(trd.q05)

TrendLongestSEG(trd.q05)


})

