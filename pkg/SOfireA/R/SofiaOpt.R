SofiaOpt <- structure(function(
	##title<< 
	## Optimize a SOFIA model using genetic optimization
	##description<<
	## The function fits a SOFIA model to observations by estimating parameters of a \code{\link{Sofia}} model. 
	
	x,
	### data.frame with independent variables
	
	area = rep(1, nrow(x)), 
	### a vector or data.frame/matrix with fractional coverage of grid cell area. If 'area' is a vector, it represents the maximal fractional burned area of a grid cell (e.g. the maximum vegetated area). If 'area' is a data.frame or matrix, it represents fractional coverage of groups (e.g. PFTs). Columns should represent groups and rows should be observations (grid cells and time steps). 
	
	per.group = rep(FALSE, ncol(x)),
	### a boolean vector that indicates if a column in x acts per group (e.g. PFTs) 
	
	sofiapar = NULL,
	### object of class \code{\link{SofiaPar}} which is used for the fit. If NULL, the argument 'par.init' is used to create sofiapar using the function \code{\link{SofiaPar}}
	
	par.init = NULL,
	### matrix of inital parameters for optimization. If NULL, inital parameter sets will be created randomly based on the parameter ranges in \code{\link{SofiaPar}}.
	
	obs,
	### a vector of observed values
	
	unc = NULL,
	### vector of observation uncertainties, if NULL an uncertainty of 1 is is used for all observations
		
	cost = NULL,
	### a function to compute the cost. If NULL, the SSE (sum of squared error) is used.
	
	pop.size = 500,
	### population size, see \code{\link{genoud}}
	
	max.generations = 30,
	### maximum number of generations, see \code{\link{genoud}}
	
	path = NULL,
	### directory for optimization results
	
	restart = 0,
	### restart: 0 = start with new optimization, 1 = start with best individuals from previous optimization in 'path', 2 = return results
	
	nodes = 5,
	### how many nodes to use for parallel executaion of genoud?
	
   BFGSburnin = max.generations - 2,
	### The number of generations before the L-BFGS-B algorithm is first used, see \code{\link{genoud}}
	
   ...
   ### further arguments to \code{\link{genoud}}

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}
) { 

   # working directory
   wd <- getwd()
   if (is.null(path)) {
      ti <- Sys.time()
      ti <- gsub(":", "", ti)
      ti <- gsub("-", "", ti)
      ti <- gsub(" ", "_", ti)
      path <- paste0(wd, "/SofiaOpt_", ti)
   }
   if (!file.exists(path)) dir.create(path)
   setwd(path)
   message(paste("SofiaOpt: directory for results:", path))
   
   
   # prepare parameters
   #-------------------
   
   # default parameters 
   if (is.null(sofiapar)) sofiapar <- SofiaPar(colnames(x), per.group = per.group, group.names = colnames(area))
   sofiapar$prior[sofiapar$prior == 0] <- 0.0000001
   npar <- length(sofiapar$names)
   par.prior <- sofiapar$prior
   
   # lower and upper parameter ranges
   if (any(is.na(sofiapar$lower)) | any(is.na(sofiapar$upper))) {
      sofiapar$lower[grepl(".max", sofiapar$names)] <- 0.0000001
      sofiapar$upper[grepl(".max", sofiapar$names)] <- par.prior[grepl(".max", sofiapar$names)] * 3
      sofiapar$lower[grepl(".sl", sofiapar$names)] <- par.prior[grepl(".sl", sofiapar$names)] * -3
      sofiapar$upper[grepl(".sl", sofiapar$names)] <- par.prior[grepl(".sl", sofiapar$names)] * 3
      sofiapar$lower[grepl("x0", sofiapar$names)] <- par.prior[grepl(".x0", sofiapar$names)] * -3
      sofiapar$upper[grepl("x0", sofiapar$names)] <- par.prior[grepl(".x0", sofiapar$names)] * 3
   }
   lu <- cbind(sofiapar$lower, sofiapar$upper)
   sofiapar$lower <- apply(lu, 1, min)
   sofiapar$upper <- apply(lu, 1, max)
   
   # parameter factors
   dpar.lower <- sofiapar$lower / par.prior
   dpar.upper <- sofiapar$upper / par.prior
   dpar <- cbind(dpar.lower, dpar.upper)
   dpar.lower <- apply(dpar, 1, min)
   dpar.upper <- apply(dpar, 1, max)
  
   
   # create initial parameter sets 
   #------------------------------
   
   # initial parameters 
   if (is.null(par.init)) {
      par.init <- par.prior 
      
      # first initals: prior, lower, and upper
      dpar.init <- rbind(par.init / par.prior, dpar.lower, dpar.upper)
      dpar.init <- rbind(dpar.init, matrix(1, nrow=200, ncol=ncol(dpar.init)))
      
      # add random initials for maximum between lower and upper
      b <- grep(".max", sofiapar$names)
      dpar.init[, b] <- apply(dpar.init[, b], 2, function(x) {
         c(x[1:3], runif(200, x[2], x[3]))
      })
      
      # add random initials for slope over large range (beyond min and max)
      b <- grep(".sl", sofiapar$names)
      dpar.init[, b] <- apply(dpar.init[, b], 2, function(x) {
         x2 <- c(runif(50, x[2], x[3]), runif(50, x[2]*2, x[3]*2), runif(50, x[2]*10, x[3]*10), runif(50, x[2]*100, x[3]*100))
         x2 <- x2 * sample(c(-1, 1), length(x2), replace=TRUE)
         c(x[1:3], x2)
      })
      
      # add random initials for x0 over large range (beyond min and max)
      b <- grep(".x0", sofiapar$names)
      dpar.init[, b] <- apply(dpar.init[, b], 2, function(x) {
         x2 <- c(runif(100, x[2], x[3]), runif(100, x[2]*3, x[3]*3))
         c(x[1:3], x2)
      })
   } else {
      dpar.init <- par.init / par.prior
   }
   
   # prepare for restart
   if (restart == 1) {
      files.result <- list.files(pattern="SofiaOpt")
      if (length(files.result) > 0) {
         message("SofiaOpt: read from previous results for restart")
         result <- ReadSofiaOpt(files.result, combine=TRUE)
         
         # get experiments with lowest cost
         err <- unlist(llply(result, function(l) l$cost))
         best <- err < quantile(err, 0.1, na.rm=TRUE)
         result <- result[best]
         err <- unlist(llply(result, function(l) l$cost))
         smpl <- c(which.min(err), sample(1:length(err), min(c(10, length(err)))))        
         dpar.init <- rbind(dpar.init, laply(result[smpl], function(l) l$dpar))
      } 
   }
   
   
   # settings for optimizer
   #-----------------------
   
   # default uncertainties
   nobs <- length(obs)  # number of observations
   if (is.null(unc)) unc <- rep(1, nobs)
   
   # default cost function
   SSE <- function(sim, obs, unc) {
      unc[unc == 0] <- 1e-8 # to avoid division by 0
      x <- na.omit(cbind(sim, obs, unc))
      sum((x[,1] - x[,2])^2 / x[,3]^2)
   }
   if (is.null(cost)) cost <- SSE
   
   # function to do the model fit, save results, and to compute the error
   .error <- function(dpar, x, area, per.group, obs, unc) {
      
      # run Sofia
      sofiapar$par <- dpar * par.prior
      sf <- Sofia(x, area, per.group=per.group, sofiapar=sofiapar, return.all=FALSE)
      #sf$data$obs <- obs
      npar <- sf$npar
      sim <- sf$data$y
      nobs <- length(obs)
      
      # compute cost
      err <- do.call(cost, list(sim=sim, obs=obs, unc=unc))
      
      # compute AIC
      sse <- SSE(sim, obs, unc)
      ll <- exp(-sse)  # likelihood
      aic <- 2 * npar - 2 * log(ll) # Akaike Information Criterion
      bic <- log(nobs) * npar - 2 * log(ll) # Bayesian Information Criterion
      
      # objective functions
      obj <- ObjFct(sim, obs)

      # save result
      file <- tempfile("genoud_", tmpdir=path, "_SofiaOpt0.RData")
      sf$data <- NULL
      sf$par <- NULL
      result <- list(list(par=sofiapar, dpar=dpar, cost=err, sse=sse, ll=ll, aic=aic, bic=bic, obj=obj, sofia=sf))
      class(result) <- "SofiaOpt"
      save(result, file=file)
      
      # combine result files
		files.result <- list.files(pattern="_SofiaOpt0.RData")
		if (length(files.result) == 200) {
			result <- ReadSofiaOpt(files.result)
			#save(result, file=tempfile("genoud_", tmpdir=getwd(), "_SofiaOpt.RData"))
		}
		
		# return error
      return(err)
   }
   #.error(dpar.init[1,], x, area, per.group, obs, unc) 
   
   # prior
   sofiapar$par <- sofiapar$prior
   sf <- Sofia(x, area, per.group=per.group, sofiapar=sofiapar)
   save(sf, file="sofiaopt_prior.RData")
   
   # plot histogram and response functions
   pdf("responses_prior.pdf", width=5, height=5)
   plot(sf, mfrow=c(1,1))
   dev.off()
  
   
   # do optimization
   #----------------
   
   opt <- NULL
   if (restart < 2) {
      
      # initialize cluster
      parallel <- cluster <- FALSE
      if (nodes > 1) {
         parallel <- TRUE
	      cluster <- makeCluster(nodes)
			
		   # load packages on all nodes
		   clusterEvalQ(cluster, {
			   library(plyr)
			   NULL
		   })
			
		   # export required objects to nodes
		   clusterExport(cluster, c("Sofia", "SofiaPar", "SofiaLogistic", "x", "area", "obs", "unc", "per.group", ".error", "cost", "SSE", "ObjFct", "ReadSofiaOpt", "path", "par.prior"), envir=environment())
		   #clusterExport(cluster, c("Sofia", "SofiaPar", "Logistic", ".error", "cost", "SSE", "ObjFct", "ReadSofiaOpt", "path", "par.prior"), envir=environment())

		   message(paste("SofiaOpt: Finished preparing cluster nodes for parallel computing.", Sys.time()))
      } 
      
      # perform genetic optimization
      message(paste("SofiaOpt: Starting genoud optimization.", Sys.time()))
      opt <- genoud(.error, nvars=npar, starting.values=dpar.init, pop.size=pop.size, max.generations=max.generations, boundary.enforcement=0, print.level=1, gradient.check=FALSE, hessian=FALSE, optim.method="BFGS", BFGSburnin=BFGSburnin, cluster=cluster, x=x, area=area, per.group=per.group, obs=obs, unc=unc)
      
      # Domains=cbind(dpar.lower, dpar.upper), boundary.enforcement=2
      
      # stop cluster
      if (parallel) stopCluster(cluster)
   
   } # end if restart
   
   
   # prepare and plot results
   #-------------------------
   
   message(paste("SofiaOpt: Prepare results.", Sys.time()))
   
   files.result <- list.files(pattern="SofiaOpt")
   result <- ReadSofiaOpt(files.result, combine=TRUE)
#   save(result, file=tempfile("genoud_", tmpdir=path, "_result.RData"))

   # get experiments with lowest cost
   result.cost <- unlist(llply(result, function(l) l$cost))
   best <- which.min(result.cost)
   
   # run best again to get all outputs
   par.best <- result[[best]]$par
   sf <- Sofia(x, area, per.group=per.group, sofiapar=par.best)
   sf$data$obs <- obs
   save(sf, file="sofiaopt_best.RData")
   
   # plot iterations
   pdf("iterations.pdf", width=7, height=6)
   plot(result)
   mtext(sf$eq, 3, 0, outer=TRUE, font=2, cex=0.8)
   mtext(path, 3, 1.3, outer=TRUE, cex=0.6)
   dev.off()
   
   # plot histogram and response functions
   pdf("responses_best.pdf", width=5, height=5)
   plot(sf, mfrow=c(1,1))
   dev.off()
   
   # plot sim vs. obs
   pdf("evaluation.pdf", width=6, height=6)
   ScatterPlot(sf$data$obs, sf$data$y, objfct=TRUE, ylab="predicted", xlab="observed", main=sf$eq)
   WollMilchSauPlot(data.frame(obs=sf$data$obs, sim=sf$data$y))
   dev.off()
   
   return(sf)
   ### an object of class 'Sofia' which is actually a list.
}, ex=function() {

require(ModelDataComp)

# example based on artificial data
#---------------------------------

# some example data
n <- 500
sm <- runif(n, 0, 100) # soil moisture
temp <- rnorm(n, 12, 10) # temperature
tree <- runif(n, 0, 1) # fractional tree cover
grass <- 1 - tree # fractional grass cover
area <- cbind(tree, grass)
x <- cbind(sm, temp)

# create 'observations'
sofiapar <- SofiaPar(colnames(x), colnames(area), per.group=c(TRUE, FALSE))
sofiapar$par <- c(1, 1, 20, 2, 1, -0.2, -0.1, 13, 10) # actual parameters
sf <- Sofia(x, area, per.group=c(TRUE, FALSE), sofiapar=sofiapar)
plot(sf) # fitted values vs. temperature
obs <- sf$data$y # 'observations'

# re-estimate parameters
path <- paste0(getwd(), "/SofiaOpt_test1") # directory for optimization outputs
par.init <- sofiapar$par * 1.5 # some inital parameters for optimization
sfbest <- SofiaOpt(x, area, per.group=c(TRUE, FALSE), obs=obs, sofiapar=sofiapar, 
 par.init=par.init, pop.size=10, max.generations=10, BFGS=FALSE, path=path, nodes=1) 
str(sfbest)
plot(sfbest)

# plot iterations of optimization
files <- list.files(pattern="SofiaOpt")
fit <- ReadSofiaOpt(files)
plot(fit)
plot(fit, plot.objfct = c("Cor", "Pbias", "RMSE"))

# compare retrieved with original parameters
sfbest$par$par / par.init

# compare retrieved vs. real
sim <- sfbest$data$y
ScatterPlot(obs, sim, objfct=TRUE)
ObjFct(sim, obs)

# compare real and retrieved response functions
plot(sf$data$x.temp, sf$data$f.temp)
points(sfbest$data$x.temp, sfbest$data$f.temp, col="red")

plot(sf$data$x.sm, sf$data$f.sm.tree)
points(sfbest$data$x.sm, sfbest$data$f.sm.tree, col="red")

plot(sf$data$x.sm, sf$data$f.sm.grass)
points(sfbest$data$x.sm, sfbest$data$f.sm.grass, col="red")


# example based on real data
# This example is commented because it needs some time.
#------------------------------------------------------

#data(firedata)

## use only training subset 
#train <- firedata$train == 1 

## predictor variables
#xvars.df <- data.frame(
#   GFED.BA.obs = firedata$GFED.BA.obs[train],
#   regid = firedata$regid[train],
#   Tree = firedata$CCI.LC.Tree[train],
#   Shrub = firedata$CCI.LC.Shrub[train],
#   HrbCrp = firedata$CCI.LC.HrbCrp[train],
#   NLDI = firedata$NLDI[train],
#   CRU.WET.orig = firedata$CRU.WET.orig[train],
#   Liu.VOD.annual = firedata$Liu.VOD.annual[train],
#   GIMMS.FAPAR.pre = firedata$GIMMS.FAPAR.pre[train],
#   CRU.DTR.orig = firedata$CRU.DTR.orig[train]
#   
#)
#xvars.df <- na.omit(xvars.df)
#obs <- xvars.df$GFED.BA.obs
#regid <- xvars.df$regid
#area <- xvars.df[,3:5]
#xvars.df <- xvars.df[,-(1:5)]

## Which x variable should depend on land cover?
#per.group <- c(FALSE, TRUE, TRUE, TRUE, TRUE)

## create parameters
#sofiapar <- SofiaPar(colnames(xvars.df), colnames(area), per.group=per.group, 
#   par.act=c(1.9, 780, 1, # for NLDI
#   0.3, 1.1, -5.3, 8.9, 0.54, -23, -39, 13, -16, # for CRU.DTR
#   0.13, 3, 0.53, 0.35, -0.44, 0.36, -1.2, -4.8, -45, # for CRU.WET
#   -0.7, 18, -1.5, 22, 11, -17, -2.3, 0.64, 1,  # for GIMMS.FAPAR
#   1.9, 3, -0.36, -21, 68, -38, 0.35, 0.31, 0.11) # for Liu.VOD
#   )

## run prior model
#sf <- Sofia(xvars.df, area, per.group=per.group, sofiapar=sofiapar)
#plot(sf) 
#ScatterPlot(obs, sf$data$y, regid, objfct=TRUE, fits="lm")

## optimize model: 
## Note that pop.size should be higher for real applications
#path <- paste0(getwd(), "/SofiaOpt_test2") # directory for optimization outputs
#par.init <- sofiapar$par.act # some inital parameters for optimization
#sfbest <- SofiaOpt(xvars.df, area, per.group=per.group, obs=obs, sofiapar=sofiapar, 
# path=path, par.init=par.init, pop.size=100, max.generations=30, BFGS=FALSE, nodes=1) 
#plot(sfbest)
#ScatterPlot(obs, sfbest$data$y, regid, objfct=TRUE, fits="lm")

})


