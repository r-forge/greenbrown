Logistic <- structure(function(
	##title<< 
	## Logistic function
	##description<<
	## Compute the result of a logistic function and a predictor variable x: \cr
	## y = mx / (1 + exp(-sl * (x - x0)))
	
	par, 
	### parameters of the logistic function, a vector of length 3 (asymptote mx, slope sl, turning point x0)
	
	x, 
	### independent variable
   
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{FitLogistic}}
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

