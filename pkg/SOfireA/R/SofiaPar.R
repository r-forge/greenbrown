SofiaPar <- structure(function(
	##title<< 
	## Parameters for SOFIA models
	##description<<
	## The function creates an object of class 'SofiaPar' (which is actually a list) which contains information about Sofia model parameters.
	
	x.names, 
	### names of independent variables
	
	per.group = rep(FALSE, length(x.names)), 
	### a boolean vector that indicates if a column in x acts per group (e.g. PFTs) 
	
	group.names = NULL,
	### names of groups
	
	par.act = NULL,
	## actual parameters
	
	par.prior = NULL,
	### prior parameters
	
	par.lower = NULL,
	### lower parameter limits
	
	par.upper = NULL,
	### upper parameter limits
	
	par.priorsd = NULL,
	### uncertainty of prior parameters
	
	par.optim = NULL,
	### (boolean) parameters that shoud be included in optimization
   
   ...
   ### further arguments

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}, \code{\link{SofiaLogistic}}
) { 
   # global parameters
   if (all(per.group)) {
      par.names <- NULL
      par.global <- NULL
   } else {
      par.names <- paste(rep(x.names[!per.group], each=4), c("max", "x0", "sl", "min"), sep=".")
      par.names <- sort(par.names)
      par.global <- rep(TRUE, length(par.names))
   }

   # group-dependent parameters
   if (any(per.group) & !is.null(group.names)) {
      ngroup <- length(group.names)
      par.group <- paste(rep(x.names[per.group], each=4), c("max", "x0", "sl", "min"),  sep=".")
      par.group <- paste(rep(par.group, ngroup), rep(group.names, each=length(par.group)), sep=".")
      par.group <- sort(par.group)
      par.names <- c(par.names, par.group)
      par.global <- c(par.global, rep(FALSE, length(par.group)))
   }
   npar <- length(par.names)
   
   # dummy parameter values
   par.dummy <- rep(NA, npar)
   par.dummy[grep("max", par.names)] <- 1
   par.dummy[grep("x0", par.names)] <- 0
   par.dummy[grep("sl", par.names)] <- 0.5
   par.dummy[grep("min", par.names)] <- 0
   
   if (is.null(par.priorsd)) par.priorsd <- rep(NA, npar)
   if (is.null(par.lower)) par.lower <- rep(NA, npar)
   if (is.null(par.prior)) par.prior <- par.dummy
   if (is.null(par.act)) par.act <- par.dummy
   if (is.null(par.upper)) par.upper <- rep(NA, npar)
   if (is.null(par.optim)) par.optim <- rep(TRUE, npar)
   
   sofiapar <- list(
      names = par.names, dummy = par.dummy, par = par.act, prior = par.prior, lower = par.lower, upper = par.upper, priorsd = par.priorsd, global = par.global, optim = par.optim, group.names = group.names)
   class(sofiapar) <- "SofiaPar"
   sofiapar$names <- as.character(sofiapar$names)
   return(sofiapar)
   ### An object of class 'SofiaPar', which is actually a list.
}, ex=function() {

# explanatory variables
sm <- 1:100
temp <- rnorm(100, 12, 10)
x <- cbind(sm, temp)

# fractional coverage of groups, e.g. plant functional types
tree <- runif(100, 0, 0.8)
grass <- 1 - tree
area <- cbind(tree, grass)

# parameters for SOFIA models
par <- SofiaPar(colnames(x), per.group=c(TRUE, FALSE), group.names=c("tree", "grass"))
par

})
