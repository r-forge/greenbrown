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

	hessian = FALSE,
	### compute standard errors of parameters based on the Hessian?
	
	plot = FALSE,
	### plot iterations for logistic fit?
	
	ninit = 100,
	### number of inital parameter sets from which to start optimization
	
	...
	### further arguments (currently not used)
	
	##references<< 
	## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.
		
	##seealso<<
	## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}} 

) {
    n <- length(x)
    avg <- mean(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    mn <- min(x, na.rm = TRUE)
    ampl <- mx - mn
    .doubleLog <- function(par, t) {
        m1 <- par[1]
        m2 <- par[2]
        m3 <- par[3]
        m4 <- par[4]
        m5 <- par[5]
        m6 <- par[6]
        m7 <- par[7]
        m3l <- m3/m4
        m4l <- 1/m4
        m5l <- m5/m6
        m6l <- 1/m6
        xpred <- m1 + (m2 - m7 * t) * ((1/(1 + exp((m3l - t)/m4l))) - 
            (1/(1 + exp((m5l - t)/m6l))))
        return(xpred)
    }
    .error <- function(par, x, t) {
        if (any(is.infinite(par))) 
            return(99999)
        xpred <- .doubleLog(par, t = t)
        sse <- sum((xpred - x)^2, na.rm = TRUE)
        return(sse)
    }
    doy <- quantile(t, c(0.25, 0.75), na.rm = TRUE)
    prior <- rbind(c(mn, mx - mn, 200, 1.5, 300, 1.5, 0.002), 
        c(mn, mx - mn, 100, 0.5, 200, 0.9, 0.002), c(mn, mx - 
            mn, 50, 0.5, 300, 1.2, 0.05), c(mn, mx - mn, 300, 
            2, 350, 2.5, 0.05))
    prior <- rbind(prior,
      apply(prior, 2, function(x) rnorm(ninit-4, mean(x), sd(x)*2))
    )
    if (plot) 
        plot(t, x)
    opt.l <- apply(prior, 1, optim, .error, x = x, t = t, method = "BFGS", 
        control = list(maxit = 100), hessian = FALSE)
    opt.df <- cbind(cost = unlist(llply(opt.l, function(opt) opt$value)), 
        convergence = unlist(llply(opt.l, function(opt) opt$convergence)), 
        ldply(opt.l, function(opt) opt$par))
    best <- which.min(opt.df$cost)
    if (opt.df$convergence[best] == 1) {
        opt <- opt.l[[best]]
        opt <- optim(opt.l[[best]]$par, .error, x = x, t = t, 
            method = "BFGS", control = list(maxit = 700), hessian = FALSE)
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t)
    } else if (opt.df$convergence[best] == 0) {
        opt <- opt.l[[best]]
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t)
    }
    if (hessian) {
        opt <- optim(opt$par, .error, x = x, t = t, method = "BFGS", 
            hessian = TRUE)
        .qr.solve <- function(a, b, tol = 1e-07, LAPACK = TRUE) {
            if (!is.qr(a)) 
                a <- qr(a, tol = tol, LAPACK = LAPACK)
            nc <- ncol(a$qr)
            nr <- nrow(a$qr)
            if (a$rank != min(nc, nr)) 
                stop("singular matrix 'a' in solve")
            if (missing(b)) {
                if (nc != nr) 
                  stop("only square matrices can be inverted")
                b <- diag(1, nc)
            }
            res <- try(qr.coef(a, b), silent=TRUE) 
            if (class(res) == "try-error") {
               res <- matrix(NA, ncol(b), nrow(b))
            }
            res
        }
        vc <- .qr.solve(opt$hessian)
        npar <- nrow(vc)
        s2 <- opt$value^2/(n - npar)
        std.errors <- sqrt(diag(vc) * s2)
        unc.alg <- "Standard errors computed from Hessian."
        
        # compute standard errors with backup algorithm
        if (all(is.na(std.errors))) {
            opt.df <- opt.df[order(opt.df$cost), ]
            par.m <- rbind(opt.df[1:5, -(1:2)], opt$par)
            v <- apply(par.m, 2, var)
            std.errors <- sqrt(v * s2)
            message("Standard errors computed with backup algorithm.")
            unc.alg <- "Standard errors computed from variance of best optimization trials"
        }
        names(std.errors) <- paste("m", 1:7, sep = "")
    }
#    if (opt$convergence != 0) {
#        opt$par[] <- NA
#        xpred <- rep(NA, length(tout))
#    } else {
        xpred <- .doubleLog(opt$par, tout)
#    }
    xpred.out <- zoo(xpred, order.by = tout)
    if (plot) {
        llply(opt.l, function(opt) {
            xpred <- .doubleLog(opt$par, t)
            lines(t, xpred, col = "cyan")
        })
        lines(t, xpred, col = "blue", lwd = 2)
    }
    names(opt$par) <- paste("m", 1:7, sep = "")
    fit.formula <- expression(m1 + (m2 - m7 * t) * ((1/(1 + exp(((m3/m4) - 
        t)/(1/m4)))) - (1/(1 + exp(((m5/m6) - t)/(1/m6))))))
    output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula)
    if (hessian) 
        output <- list(predicted = xpred.out, params = opt$par, 
            formula = fit.formula, stdError = std.errors, stdErrorAlgorithm=unc.alg)
    return(output)
	### The function returns a list with fitted values, parameters, fitting formula and standard errors if hessian is TRUE
}, ex=function() {

# select one year of NDVI data
x <- as.vector(window(ndvi, start=c(1991,1), end=c(1991, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
fit
plot(x)
lines(fit$predicted, col="blue")

# do more inital trials, plot iterations and compute parameter uncertainties
FitDoubleLogElmore(x, hessian=TRUE, plot=TRUE, ninit=1000)	

# fit double-logistic function to one year of data, 
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout=tout)
plot(x)
lines(tout, fit$predicted, col="blue")
PhenoDeriv(fit$predicted, plot=TRUE)


})


