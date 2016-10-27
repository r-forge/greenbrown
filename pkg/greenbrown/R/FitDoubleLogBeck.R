FitDoubleLogBeck <- structure(function(
	##title<< 
	## Fit a double logisitic function to a vector according to Beck et al. (2006)
	##description<<
	## This function fits a double logistic curve to observed values using the function as described in Beck et al. (2006) (equation 3).
	
	x,
	### vector or time series to fit
	
	t = 1:length(x),
	### time steps
	
	tout = t,
	### time steps of output (can be used for interpolation)
	
	weighting = TRUE,
	### apply the weighting scheme to the observed values as described in Beck et al. 2006? This is useful for NDVI observations because higher values will get an higher weight in the estimation of the double logisitic function than lower values.	
	
	return.par = FALSE,
	### if TRUE the function returns parameters of the double logisitic fit, if FALSE it returns the fitted curve
	
	plot = FALSE,
	### plot iterations for logistic fit?
	
	...
	### further arguments (currently not used)
	
	##references<< 
	## Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.
		
	##seealso<<
	## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}} 

) {
	
	# linear interpolation if all equal
	if (AllEqual(x)) {
		if (return.par) return(rep(NA, 6))
		if (!return.par) return(rep(x[1], length(tout)))
	}
	
	# double logistic function
	.doubleLog <- function(par, t) {
		mn <- par[1]
		mx <- par[2]
		sos <- par[3]
		rsp <- par[4]
		eos <- par[5]
		rau <- par[6]
		
		# double-logisitic model according to Beck et al. 2006
		xpred <- mn + (mx - mn) * (1/(1+exp(-rsp * (t - sos))) + 1/(1+exp(rau * (t - eos))))
		return(xpred)
	}
	
	# error function
	.error <- function(par, x, weights) {
		if (any(is.infinite(par))) return(99999)
		if (par[1] > par[2]) return(99999)
		xpred <- .doubleLog(par * par.prior, t=t)
		sse <- sum((xpred - x)^2 * weights, na.rm=TRUE)
		return(sse)
	}
	
	# weights for function fit
	if (weighting) {
		iter <- 1:2
	} else {
		iter <- 1
	}	
	weights <- rep(1, length(x))	# inital weights
	
	# inital parameters to fit double-logistic function
	doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
	prior <- rbind(
		c(mn, mx, doy[1], 0.5, doy[2], 0.5),
		c(mn, mx, doy[2], 0.5, doy[1], 0.5),
		c(mn-ampl/2, mx+ampl/2, doy[1], 0.5, doy[2], 0.5),
		c(mn-ampl/2, mx+ampl/2, doy[2], 0.5, doy[1], 0.5)
	)

	if (plot) plot(t, x)
	for (i in iter) {
		# estimate parameters for double-logistic function starting at different priors
		opt.l <- apply(prior, 1, optim, .error, x=x, weights=weights, method="Nelder-Mead")	# fit from different prior values
		opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
		best <- which.min(opt.df$cost)
		
		# test for convergence
		if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
			opt <- opt.l[[best]]
			# repeat with more maximum iterations if it did not converge
			opt <- optim(opt.l[[best]]$par, .error, x=x, weights=weights, method="Nelder-Mead", control=list(maxit=500))
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
		
		# get optimized parameters
		parinit <- opt$par
		mn <- opt$par[1]
		mx <- opt$par[2]
		sos <- opt$par[3]
		rsp <- opt$par[4]
		eos <- opt$par[5]
		rau <- opt$par[6]
		m <- lm(c(0, 100) ~ c(sos, eos))
		tr <- coef(m)[2] * 1:length(x) + coef(m)[1]
		tr[tr < 0] <- 0
		tr[tr > 100] <- 100
				
		# estimate weights
		res <- xpred - x
		weights <- 1/((tr * res + 1)^2)
		weights[res > 0 & res <= 0.01] <- 1
		weights[res < 0] <- 4
	}
	
	# return NA in case of no convergence
	if (opt$convergence != 0) {
		opt$par[] <- NA
		xpred <- rep(NA, length(tout))
	} else {
		xpred <- .doubleLog(opt$par, tout)
	}	
	
	if (return.par) {
		names(opt$par) <- c("mn", "mx", "sos", "rsp", "eos", "rau")
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
x <- as.vector(window(ndvi, start=c(1996,1), end=c(1996, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogBeck(x)
plot(x)
lines(fit, col="blue")

# return parameters of fit and plot iterations
FitDoubleLogBeck(x, return.par=TRUE, plot=TRUE)	

# fit double-logistic function to one year of data, 
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogBeck(x, tout=tout)
PhenoDeriv(fit, plot=TRUE)

})


