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
		slope_unc = NA,
		pval = NA,
		pval_unc = NA,
		tau = NA,
		tau_unc = NA,
		bptest = bptest,
		method = "AAT")
	class(result) <- "Trend"
	return(result)
	### The function returns a list of class "Trend". 
})


