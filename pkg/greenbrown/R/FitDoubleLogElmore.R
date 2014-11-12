FitDoubleLogElmore <- structure(function(
	##title<< 
	## Fit a double logisitic function to a vector according to Elmore et al. (2012)
	##description<<
	## This function fits a double logistic curve to observed values using the function as described in Elmore et al. (2012) (equation 4).
	
	x,
	### vector or time series to fit
	
	t = 1:length(x),
	### time steps
	
	tout = t,
	### time steps of output (can be used for interpolation)

	return.par = FALSE,
	### if TRUE the function returns parameters of the double logisitic fit, if FALSE it returns the fitted curve
	
	plot = FALSE,
	### plot iterations for logistic fit?
	
	...
	### further arguments (currently not used)
	
	##references<< 
	## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.
		
	##seealso<<
	## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}} 

) {

	# linear interpolation if all equal
	if (AllEqual(x)) {
		if (return.par) return(rep(NA, 7))
		if (!return.par) return(rep(x[1], length(tout)))
	}
	
	# return NA if too few observations
	n <- length(na.omit(x))
	if (n < 7) {
		if (return.par) return(rep(NA, 7))
		if (!return.par) return(rep(NA, length(tout)))
	}

	# get statistical values
	n <- length(x)
	avg <- mean(x, na.rm=TRUE)
	mx <- max(x, na.rm=TRUE)
	mn <- min(x, na.rm=TRUE)
	ampl <- mx - mn
		
	# double logistic function
	.doubleLog <- function(par, t) {
		m1 <- par[1]
		m2 <- par[2]
		m3 <- par[3]
		m4 <- par[4]
		m5 <- par[5]
		m6 <- par[6]
		m7 <- par[7]
		
		m3l <- m3 / m4
		m4l <- 1 / m4
		m5l <- m5 / m6
		m6l <- 1 / m6
		xpred <- m1 + (m2 - m7 * t) * ( (1/(1 + exp((m3l - t)/m4l))) - (1/(1 + exp((m5l - t)/m6l))) )
		return(xpred)
	}
	
	# error function
	.error <- function(par, x, t) {
		if (any(is.infinite(par))) return(99999)
		xpred <- .doubleLog(par, t=t)
		sse <- sum((xpred - x)^2, na.rm=TRUE)
		return(sse)
	}
	
	# .error <- function(par, x, t) {
		# if (any(is.infinite(par))) return(99999)
		# xpred <- .doubleLog(par, t=t)
		# kge <- KGE(xpred, x) # compute Kling-Gupta efficiency with associated metrics
		# ed <- kge[2] # use the euclidean distance as cost function because we are minimizing
		# return(ed)
	# }
	
	# inital parameters to fit double-logistic function
	doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
	prior <- rbind(
		c(mn, mx-mn, doy[1], 0.5, doy[2], 0.5, 0.002),
		c(mn, mx-mn, doy[2], 0.5, doy[2], 0.5, 0.002),
		c(mn, mx-mn, doy[1], 0.5, doy[2], 0.5, 0.05),
		c(mn, mx-mn, doy[2], 0.5, doy[1], 0.5, 0.05)
	)

	if (plot) plot(t, x)
	
	# estimate parameters for double-logistic function starting at different priors
	opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=100))	# fit from different prior values
	
# ####	

	# scalars <- prior / matrix(prior[1,], ncol=ncol(prior), nrow=nrow(prior), byrow=TRUE)
	# .scale <- function(scalars, priors, x, t) {
		# par <- scalars * priors
		# cost <- .error(par, x=x, t=t)
		# return(cost)
	# }
	# opt.l <- apply(scalars, 1, optim, .scale, priors=prior[1,], x=x, t=t, method="BFGS", control=list(maxit=100))
	# opt.l <- llply(opt.l, function(opt) {
		# opt$par <- opt$par * prior[1,]
		# return(opt)
	# })
# ###
	
	opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
	best <- which.min(opt.df$cost) 
	
	# test for convergence
	if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
		opt <- opt.l[[best]]
		opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", control=list(maxit=700))
		prior <- rbind(prior, opt$par)
		xpred <- .doubleLog(opt$par, 1:length(x))			
	} else if (opt.df$convergence[best] == 0) {
		opt <- opt.l[[best]]
		prior <- rbind(prior, opt$par)
		xpred <- .doubleLog(opt$par, 1:length(x))	
	} 
	
	# plot iterations
	if (plot) {
		llply(opt.l, function(opt) {
			xpred <- .doubleLog(opt$par, 1:length(x))
			lines(t, xpred, col="cyan")
		})
		lines(t, xpred, col="blue", lwd=2)
	}
		
	# return NA in case of no convergence
	if (opt$convergence != 0) {
		opt$par[] <- NA
		xpred <- rep(NA, length(tout))
	} else {
		xpred <- .doubleLog(opt$par, tout)
	}

	
	if (return.par) {
		names(opt$par) <- paste("m", 1:7, sep="")
		return(opt$par)
	} else {
		return(xpred)
	}
	### The function returns a vector with fitted values if return.par is FALSE and it returns parameters of the logisitic modle of return.par is TRUE
}, ex=function() {

# load NDVI data
data(ndvi)
plot(ndvi)

# select one year of data
x <- as.vector(window(ndvi, start=c(1997,1), end=c(1997, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
plot(x)
lines(fit, col="blue")

# return parameters of fit
FitDoubleLogElmore(x, return.par=TRUE, plot=TRUE)	# plot=TRUE causes plotting of different double logisitic

# fit double-logistic function to one year of data, interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout=tout)
plot(x)
lines(tout, fit, col="blue")
PhenoDeriv(fit, plot=TRUE)


})


