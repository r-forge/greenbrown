RandomLHSPar <- structure(function(
	##title<<
	## Latin-hypercube sampling of parameters
	##description<<
	## RandomLHSPar samples parameters based on random Latin-Hypercube sampling within given parameter ranges
	
	nexp, 
	### number of samples
	
	lower, 
	### numeric vector of lower boundary values for each parameter
	
	upper, 
	### numeric vector of upper borundary values for each parameter
	
	fixed=rep(FALSE, length(lower)) 
	### boolean vector: should a parameter kept fixed (TRUE) or should all parameters be sampled (FALSE). If TRUE than the parameter value from the vector lower will be returned.	
) {
	require(lhs)
	# random LHS sample
	npar <- length(lower)	# number of parameters
	npar.rand <- length(fixed == FALSE)
	lhs <- randomLHS(n=nexp, k=npar.rand)

	# transform Latin hypercube sampling to distribution of parameters
	par.sample <- matrix(0, nrow=nexp, ncol=npar)
	for (i in 1:npar) {
		if (fixed[i]) {
			par.sample[,i] <- lower[i]
		} else {
			par.sample[,i] <- qunif(lhs[,i], lower[i], upper[i])
		}
	}
	param.df <- as.data.frame(par.sample)
	colnames(param.df) <- names(lower)
	return(param.df)
	### The function returns a data.frame with sampled parameters.
}, ex=function() {

lower <- c(0, 100, -50, 1000)
names(lower) <- letters[1:4]
upper <- c(10, 500, 50, 10000)
RandomLHSPar(10, lower, upper)

})

