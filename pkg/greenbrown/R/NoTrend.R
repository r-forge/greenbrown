NoTrend <- structure(function(
	##title<<
	## Initialize an empty list of trends
	
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
		pval = NA,
		bptest = bptest,
		method = "AAT")
	class(result) <- "Trend"
	return(result)
	### empty list with slot 'breakpoints'
})


