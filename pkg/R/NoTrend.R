NoTrend <- structure(function(
	##title<<
	## Initialize an empty list of trends
	
	##description<< This is an internal function to make an empty list of breakpoints. For the user there is usually no need to use this function.
	
	Yt
	### univariate time series of class \code{\link{ts}}
) {
	result <- list(
		series = rep(0, length(Yt)),
		trend = rep(0, length(Yt)),
		time = time(Yt),
		bp = NoBP(),
		slope = NA,
		pval = NA,
		bptest = NA,
		method = "AAT")
	class(result) <- "Trend"
	return(result)
	### empty list with slot 'breakpoints'
})


