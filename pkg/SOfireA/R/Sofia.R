Sofia <- structure(function(
	##title<< 
	## Satellite Observations for Fire Activity
	##description<<
	## SOFIA (Satellite Observations for Fire Activity) is an empirical modelling concept to predict burned area based on satellite and climate data. Thereby several logistic functions are multiplicatively combined.
	
	x,
	### data.frame with independent variables
	
	area = rep(1, nrow(x)), 
	### a vector or data.frame/matrix with fractional coverage of grid cell area. If 'area' is a vector, it represents the maximal fractional burned area of a grid cell (e.g. the maximum vegetated area). If 'area' is a data.frame or matrix, it represents fractional coverage of groups (e.g. PFTs). Columns should represent groups and rows should be observations (grid cells and time steps). 
	
	per.group = rep(FALSE, ncol(x)),
	### a boolean vector that indicates if a column in x acts per group (e.g. PFTs) 
	
	sofiapar = NULL,
	### object of class \code{\link{SofiaPar}} which is used for the fit. If NULL, the argument 'par' is used to create sofiapar using the function \code{\link{SofiaPar}}
		
	par = NULL,
	### vector of parameters of logistic functions. If NULL, default parameters are used (that are usually physically not plausible)
	
	return.all = TRUE,
	### return all input and results? The function returns an object of class 'Sofia'. If TRUE, this object includes in the 'data' slot the fitted values, the fits per group, the response functions, the inputs 'x' and 'area'. If FALSE, only the fitted values are included. 
	
   ...
   ### further arguments

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{SofiaOpt}}, \code{\link{Logistic}}
) { 
   require(plyr)

   # check if setup has PFT-dependent and global variables
   with.group <- !is.vector(area)
   if (!with.group) area <- matrix(area, ncol=1)
   with.global <- any(!per.group)
   group.names <- "Area"
   ngroup <- 1
   if (with.group) {
      if (ncol(area) == 1) {
         with.group <- FALSE
      } else {
         group.names <- colnames(area)
         ngroup <- length(group.names)
      }
   } 
   if (!with.group & any(per.group)) {
      stop("Cannot calculate responses per group if groups are not provided in 'area'.")
   }
      
   # check if number of observations match
   if (nrow(area) != nrow(x)) stop("Number of observations in 'x' and 'area' are not the same.")
   
   # get parameter names
   sofiapar0 <- SofiaPar(colnames(x), per.group, group.names, par)
   if (is.null(sofiapar)) {
      sofiapar <- sofiapar0
   } else {
      if (!all(sofiapar0$names == sofiapar$names)) {
         stop("Provided 'sofiapar' object does not agree with the parameters in 'Sofia'.")
      }
   }
   npar <- length(sofiapar$names)
   
   # calculate response functions for global variables
   if (with.global) {
      resp.gl <- llply(as.list(colnames(x)[!per.group]), function(xvar) {
         g1 <- c(grep(paste0(xvar, ".max"), sofiapar$names), grep(paste0(xvar, ".sl"), sofiapar$names), grep(paste0(xvar, ".x0"), sofiapar$names)) 
         g2 <- grep(xvar, colnames(x))[1]
         y.xvar.gl <- Logistic(sofiapar$par[g1], x[, g2])
         y.xvar.gl[y.xvar.gl > 1] <- 1
         y.xvar.gl[y.xvar.gl < 0] <- 0
         return(y.xvar.gl)
      })
   }
   
   # calculate response function for group-dependent variables
   if (any(per.group)) {
      # iterate over variables
      resp.gr <- llply(as.list(colnames(x)[per.group]), function(xvar) {
         # iterate over groups
         y.xvar.gr <- matrix(NA, ncol=ncol(area), nrow=nrow(area))
         for (i in 1:ngroup) {
            m <- match(c(paste(xvar, "max", group.names[i], sep="."), paste(xvar, "sl", group.names[i], sep="."), paste(xvar, "x0", group.names[i], sep=".")),  sofiapar$names)
            g2 <- grep(xvar, colnames(x))[1]
            y.xvar.gr[,i] <- Logistic(sofiapar$par[m], x[, g2])
            # plot(x[, g2], y.xvar.gr[,i])
         }
         y.xvar.gr[y.xvar.gr > 1] <- 1
         y.xvar.gr[y.xvar.gr < 0] <- 0
         return(y.xvar.gr)
      })
   }
   
   # calculate value for each group
   y.gr <- area 
   for (i in 1:ngroup) {
   
      # add global response
      if (with.global) {
         for (j in 1:length(resp.gl)) y.gr[,i] <- y.gr[,i] * resp.gl[[j]]
      }
      
      # add group-specific responses
      if (with.group & any(per.group)) {
         for (j in 1:length(resp.gr)) y.gr[,i] <- y.gr[,i] * resp.gr[[j]][,i]
      }
   }
   
   # total area = sum()
   y <- rowSums(y.gr)
   
   # create equation
   eq <- "y = "
   if (with.group) eq <- paste0(eq, "sum(A_g")
   if (!with.group) eq <- paste0(eq, "A")
   if (with.global) eq <- paste(eq, paste(paste0("* f(", colnames(x)[!per.group], ")"), collapse=" "))
   if (any(per.group)) eq <- paste(eq, paste(paste0("* f(", colnames(x)[per.group], "_g)"), collapse=" "))
   if (with.group) eq <- paste0(eq, ")")
   
   # prepare results
   if (return.all) {
      res.df <- data.frame(y=y, y=y.gr, area=area, x=x)
      if (with.global) {
         nms <- colnames(res.df)
         for (j in 1:length(resp.gl)) res.df <- cbind(res.df, resp.gl[j])
         colnames(res.df) <- c(nms, paste0("f.", colnames(x)[!per.group]))
      }
      if (with.group & any(per.group)) {
         nms <- colnames(res.df)
         for (j in 1:length(resp.gr)) {
            res.df <- cbind(res.df, resp.gr[[j]])
            nms <- c(nms, paste0(paste0("f.", (colnames(x)[per.group])[j]), ".", group.names))
         }
         colnames(res.df) <- nms
      }
   } else {
      res.df <- data.frame(y=y)
   }

   sofia <- list(par=sofiapar, data=res.df, with.group=with.group, with.global=with.global, group.names=group.names, per.group=per.group, x.names=colnames(x), eq=eq, npar=npar)
   class(sofia) <- "Sofia"
   return(sofia)
   ### an object of class 'Sofia' which is actually a list.
}, ex=function() {

# explanatory variables
sm <- 1:100
temp <- rnorm(100, 12, 10)
x <- cbind(sm, temp)

# fractional coverage of groups, e.g. plant functional types
tree <- runif(100, 0, 0.8)
grass <- 1 - tree
area <- cbind(tree, grass)

# calculate Sofia with some dummy parameters:
sf <- Sofia(x, area, per.group=c(TRUE, FALSE))
sf$eq
summary(sf$data)
plot(sf)

# with some more realisitc parameters:
par <- SofiaPar(colnames(x), per.group=c(TRUE, FALSE), group.names=c("tree", "grass"))
par
par$par <- c(1, 1, 20, 2, 1, -0.2, -0.1, 13, 10)
sf <- Sofia(x, area, per.group=c(TRUE, FALSE), sofiapar=par)
plot(sf)

sm <- 1:100
sm.2 <- sm
temp <- rnorm(100, 12, 10)
x <- cbind(sm, sm.2, temp)
par <- SofiaPar(colnames(x), per.group=c(TRUE, TRUE, FALSE), group.names=c("tree", "grass"))
par
par$par <- c(2, 1, 20, 2, 2, 0.3, 0.2, 20, 40, 1, 1, -0.2, -0.1, 20, 10)  
sf <- Sofia(x, area, per.group=c(TRUE, TRUE, FALSE), sofiapar=par)
plot(sf)


})






