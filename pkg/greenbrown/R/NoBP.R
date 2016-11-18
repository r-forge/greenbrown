NoBP <- structure(function(
	##title<<
	## Initialize an empty list with breakpoints
	
	##description<< This is an internal function to make an empty list of breakpoints. For the user there is usually no need to use this function.
) {
	bp_est <- NULL
	bp_est$breakpoints <- NA
	return(bp_est)
	### empty list with slot 'breakpoints'
})
