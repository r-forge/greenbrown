Greenup <- structure(function(
	##title<< 
	## The function identifies 'greenup' (i.e. periods with increase) and 'senescence' (i.e. periods with decrease) in time series 
	##description<<
	## This function implements threshold methods for phenology. Please use the function \code{\link{Phenology}} to apply this method.
	
	x, 
	### vector of values
	
	...
	### further arguments (currently not used)
		
	##seealso<<
	## \code{\link{Phenology}}

) {			
	# identify greenup or dormancy period
	ratio.deriv <- c(NA, diff(x))	
	greenup	<- rep(NA, length(x))
	greenup[ratio.deriv > 0] <- TRUE
	greenup[ratio.deriv < 0] <- FALSE
			
	return(greenup)
	### The function returns a boolean vector: TRUE (greenup) and FALSE (senescence). }
})


