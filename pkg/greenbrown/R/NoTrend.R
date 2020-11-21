NoTrend <- structure(function(
	##title<<
	## Initialize an empty object of class "Trend"
	
	##description<< This is an internal function to make an empty list of class Trend. For the user there is usually no need to use this function.
	
	Yt
	### univariate time series of class \code{\link{ts}}
) {
	bptest <- list(statistic=NA, p.value=NA)
	result <- list(
		series = rep(0, length(Yt)),
		trend = rep(0, length(Yt)),
		time = time(Yt),
		bp = NoBP(),	
		slope = NA, 
		slope_unc = NoUnc(),
		slope_se = NA,
		pval = NA,  
		perc = NA,
		perc_unc = NoUnc(),
		mk.tau = NA,
		mk.tau_unc = NoUnc(),
		mk.pval = NA,
		bptest = bptest,
		method = "NoTrend")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
})


NoUnc <- structure(function(
	##title<<
	## Initialize an empty data.frame for trend uncertainties
	
	##description<< This is an internal function. For the user there is usually no need to use this function.
	
	Yt
	### univariate time series of class \code{\link{ts}}
) {
	return(data.frame(.id=1, NA, NA, NA))
	### The function returns a data.frame. 
})


