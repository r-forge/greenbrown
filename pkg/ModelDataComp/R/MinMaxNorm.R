MinMaxNorm <- structure(function(
	##title<< 
	## Normalize values between to a range
	##description<<
	## Normalizes values to a given range.
	
	x,
	### values
	
	ext=c(0,1),
	### new minimum and maximum values
	
   ...
   ### further arguments (unused)

	##details<<
	## No details.
	
	##references<< No reference.	
) {  
	max <- max(x, na.rm=TRUE)
	min <- min(x, na.rm=TRUE)
	std <- (x - min) * ((ext[2] - ext[1]) / (max - min)) + ext[1]
	return(std)
}, ex=function() {

x <- rnorm(10, 0, 2)
n <- MinMaxNorm(x)
plot(x, n)

})
