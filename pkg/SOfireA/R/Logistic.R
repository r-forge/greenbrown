Logistic <- structure(function(
	##title<< 
	## Logistic function
	##description<<
	## Compute values of a logistic function.
	
	par, 
	### parameters of logistic function, a vector of length 3 (asymptote, slope, turning point)
	
	x, 
	### independent variable
   
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}
) { 
   y <- par[1] / (1 + exp(-par[2] * (x - par[3])))
   return(y)
   ### a vector
}, ex=function() {
x <- -20:20
par <- c(1, 0.5, 0)
plot(x, Logistic(par, x), type="l")

par <- c(1, 0.2, 0)
plot(x, Logistic(par, x), type="l")

par <- c(10, -1, 0)
plot(x, Logistic(par, x), type="l")

par <- c(-2, -1, 0)
plot(x, Logistic(par, x), type="l")

})

