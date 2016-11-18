\name{Sofia}
\alias{Sofia}
\title{Satellite Observations for Fire Activity}
\description{SOFIA (Satellite Observations for Fire Activity) is an empirical modelling concept to predict burned area based on satellite and climate data. Thereby several logistic functions are multiplicatively combined.}
\usage{Sofia(x, area = rep(1, nrow(x)), per.group = rep(FALSE, ncol(x)), 
    sofiapar = NULL, par = NULL, return.all = TRUE, ...)}
\arguments{
  \item{x}{data.frame with independent variables}
  \item{area}{a vector or data.frame/matrix with fractional coverage of grid cell area. If 'area' is a vector, it represents the maximal fractional burned area of a grid cell (e.g. the maximum vegetated area). If 'area' is a data.frame or matrix, it represents fractional coverage of groups (e.g. PFTs). Columns should represent groups and rows should be observations (grid cells and time steps). }
  \item{per.group}{a boolean vector that indicates if a column in x acts per group (e.g. PFTs) }
  \item{sofiapar}{object of class \code{\link{SofiaPar}} which is used for the fit. If NULL, the argument 'par' is used to create sofiapar using the function \code{\link{SofiaPar}}}
  \item{par}{vector of parameters of logistic functions. If NULL, default parameters are used (that are usually physically not plausible)}
  \item{return.all}{return all input and results? The function returns an object of class 'Sofia'. If TRUE, this object includes in the 'data' slot the fitted values, the fits per group, the response functions, the inputs 'x' and 'area'. If FALSE, only the fitted values are included. }
  \item{\dots}{further arguments}
}
\details{No details.}
\value{an object of class 'Sofia' which is actually a list.}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SofiaOpt}}, \code{\link{SofiaLogistic}}}
\examples{


# example based on aritifical data
#---------------------------------

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
par <- SofiaPar(colnames(x), per.group=c(TRUE, TRUE, FALSE), 
   group.names=c("tree", "grass"))
par
par$par <- c(2, 1, 20, 2, 2, 0.3, 0.2, 20, 40, 1, 1, -0.2, -0.1, 20, 10)  
sf <- Sofia(x, area, per.group=c(TRUE, TRUE, FALSE), sofiapar=par)
plot(sf)


# example based on real data
#---------------------------

require(ModelDataComp)

# get data
data(firedata)

# predictor variables
train <- firedata$train == 1 # use training data
xvars.df <- data.frame(
   NLDI = firedata$NLDI[train],
   CRU.WET.orig = firedata$CRU.WET.orig[train],
   Liu.VOD.annual = firedata$Liu.VOD.annual[train],
   GIMMS.FAPAR.pre = firedata$GIMMS.FAPAR.pre[train],
   CRU.DTR.orig = firedata$CRU.DTR.orig[train]
)

# observed data
obs <- firedata$GFED.BA.obs[train]
regid <- firedata$regid[train]

# Which x variable should depend on land cover?
per.group <- c(FALSE, TRUE, TRUE, TRUE, TRUE)

# land cover
area <- data.frame(
   Tree = firedata$CCI.LC.Tree[train],
   Shrub = firedata$CCI.LC.Shrub[train],
   HrbCrp = firedata$CCI.LC.HrbCrp[train]
)

# define parameters (from Forkel et al. 2016, Fig. 1)
sofiapar <- SofiaPar(colnames(xvars.df), colnames(area), per.group=per.group, 
   par.act=c(1.9, 780, 1, # for NLDI
   0.3, 1.1, -5.3, 8.9, 0.54, -23, -39, 13, -16, # for CRU.DTR
   0.13, 3, 0.53, 0.35, -0.44, 0.36, -1.2, -4.8, -45, # for CRU.WET
   -0.7, 18, -1.5, 22, 11, -17, -2.3, 0.64, 1,  # for GIMMS.FAPAR
   1.9, 3, -0.36, -21, 68, -38, 0.35, 0.31, 0.11) # for Liu.VOD
   )

# run model
sf <- Sofia(xvars.df, area, per.group=per.group, sofiapar=sofiapar)
plot(sf) 


}
