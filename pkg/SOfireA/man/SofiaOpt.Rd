\name{SofiaOpt}
\alias{SofiaOpt}
\title{Optimize a SOFIA model using genetic optimization}
\description{The function fits a SOFIA model to observations by estimating parameters of a \code{\link{Sofia}} model. }
\usage{SofiaOpt(x, area = rep(1, nrow(x)), per.group = rep(FALSE, ncol(x)), 
    sofiapar = NULL, par.init = NULL, obs, unc = NULL, cost = NULL, 
    pop.size = 500, max.generations = 30, path = NULL, restart = 0, 
    nodes = 5, BFGSburnin = max.generations - 2, ...)}
\arguments{
  \item{x}{data.frame with independent variables}
  \item{area}{a vector or data.frame/matrix with fractional coverage of grid cell area. If 'area' is a vector, it represents the maximal fractional burned area of a grid cell (e.g. the maximum vegetated area). If 'area' is a data.frame or matrix, it represents fractional coverage of groups (e.g. PFTs). Columns should represent groups and rows should be observations (grid cells and time steps). }
  \item{per.group}{a boolean vector that indicates if a column in x acts per group (e.g. PFTs) }
  \item{sofiapar}{object of class \code{\link{SofiaPar}} which is used for the fit. If NULL, the argument 'par.init' is used to create sofiapar using the function \code{\link{SofiaPar}}}
  \item{par.init}{matrix of inital parameters for optimization. If NULL, inital parameter sets will be created randomly based on the parameter ranges in \code{\link{SofiaPar}}.}
  \item{obs}{a vector of observed values}
  \item{unc}{vector of observation uncertainties, if NULL an uncertainty of 1 is is used for all observations}
  \item{cost}{a function to compute the cost. If NULL, the SSE (sum of squared error) is used.}
  \item{pop.size}{population size, see \code{\link{genoud}}}
  \item{max.generations}{maximum number of generations, see \code{\link{genoud}}}
  \item{path}{directory for optimization results}
  \item{restart}{restart: 0 = start with new optimization, 1 = start with best individuals from previous optimization in 'path', 2 = return results}
  \item{nodes}{how many nodes to use for parallel executaion of genoud?}
  \item{BFGSburnin}{The number of generations before the L-BFGS-B algorithm is first used, see \code{\link{genoud}}}
  \item{\dots}{further arguments to \code{\link{genoud}}}
}
\details{No details.}
\value{an object of class 'Sofia' which is actually a list.}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}}
\examples{

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

}
