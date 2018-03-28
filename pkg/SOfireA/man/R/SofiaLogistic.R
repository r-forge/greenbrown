SofiaLogistic <- structure(function(
	##title<< 
	## Logistic function 
	##description<<
	## Compute values of a logistic function as used in Sofia models.
	
	par, 
	### parameters of logistic function, a vector of length 3 (upper asymptote, slope, turning point) or length 4 (upper asymptote, slope, turning point, lower asymptote)
	
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
   # version 1
   #y <- par[1] / (1 + exp(-par[2] * (x - par[3]))) 
   
   # version 2 with par[4] as lower asymptote
   if (is.na(par[4])) par[4] <- 0
   y <- (par[1] - par[4]) / (1 + exp(-par[2] * (x - par[3]))) + par[4] 
   y[y > 1] <- 1
   y[y < 0] <- 0
   return(y)
   ### a vector
}, ex=function() {
x <- -20:20
par <- c(1, 0.5, 0)
plot(x, SofiaLogistic(par, x), type="l")

par <- c(1, 0.2, 0)
plot(x, SofiaLogistic(par, x), type="l")

par <- c(10, -1, 0)
plot(x, SofiaLogistic(par, x), type="l")

# define also the lower asymptote as forth parameter
par <- c(1, 0.5, 0, 0.1)
plot(x, SofiaLogistic(par, x), type="l")

par <- c(1, 0.5, 0, -1)
plot(x, SofiaLogistic(par, x), type="l")

})

