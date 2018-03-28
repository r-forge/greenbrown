FitSofia <- structure(function(
	##title<< 
	## Fit a Sofia model to a data set
	##description<<
	## The function fits a SOFIA model to a dataset. 
	
	x, 
	### data.frame with independent variables
	
	y, 
	### dependent variable (observation)
	
	unc = NULL,
	### uncertainty of dependent variable
		
	per.group = rep(FALSE, ncol(x)),
	### a boolean vector that indicates if a column in x acts per group (e.g. PFTs) 
   
	nodes=4, 
	### number of nodes for parallel compuation during genetic optimization
	
	sofiapar,
	### SofiaPar object with prior parameters
	
	restart=0,
	### restart previous Sofia optimization? 0 = start new, 1 = continue with previous, 2 = do post-processing
	
	cost = NULL,
	### cost function to be used
	
	...
	### further arguments

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}, \code{\link{SofiaOpt}}
) { 
   
   # get area
   area <- x[, match(sofiapar$group.names, colnames(x))]
   
   # get data
   x <- x[, -match(sofiapar$group.names, colnames(x))]
   
   # check lower and upper ranges
   lower <- apply(cbind(sofiapar$lower, sofiapar$upper), 1, min)
   upper <- apply(cbind(sofiapar$lower, sofiapar$upper), 1, max)
   sofiapar$lower <- lower
   sofiapar$upper <- upper

   # define cost function
   if (is.null(cost)) cost <- function(sim, obs, ...) ObjFct(sim, obs)$SSE # cost function
   
   max.generations <- 25
   BFGSburnin <-  max.generations - 3
   
   # run optimization
   message("FitSofia: optimize Sofia model.")
   model <- SofiaOpt(x, area, per.group, sofiapar = sofiapar, obs=y, unc=unc, cost = cost, pop.size = 500, max.generations = max.generations, path = getwd(), BFGSburnin=BFGSburnin, restart = restart, nodes = nodes)
   
   # files <- list.files(pattern="SofiaOpt")
   # fit <- ReadSofiaOpt(files, combine=FALSE)
   # plot(fit)
   
   return(model)
})




