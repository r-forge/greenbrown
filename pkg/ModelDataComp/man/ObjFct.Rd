\name{ObjFct}
\alias{ObjFct}
\title{Objective functions}
\description{Calculate model performance measures: correlation coefficient, coefficient of determination, average error, 
normalized average error, fractional mean bias, variance ratio, Pbias, sum squared error, mean squared error,
root mean squared error, normalized root mean squared error, index of agreement, mean absolute error, 
normalized mean absolute error, maximum absolute error, median absolute error, upper quartile absolute error,
ratio of scatter, Modelling efficiency, Kling-Gupta efficiency}
\usage{ObjFct(sim, obs, groups = NULL)}
\arguments{
  \item{sim}{simulated/predicted/modeled values}
  \item{obs}{observed/reference values}
  \item{groups}{vector of groups to compute objective functions for subsets of sim/obs}
}

\value{An object of class "ObjFct" which is actually a list.}
\references{Gupta, H. V., H. Kling, K. K. Yilmaz, and G. F. Martinez (2009), Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling, Journal of Hydrology, 377(1-2), 80–91, doi:10.1016/j.jhydrol.2009.08.003. \cr
Janssen, P. H. M., and P. S. C. Heuberger (1995), Calibration of process-oriented models, Ecological Modelling, (83), 55-66. \cr
Krause, P., D. P. Boyle, and F. Bäse (2005), Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, doi:10.5194/adgeo-5-89-2005. \cr
Legates, D. R., and G. J. McCabe (1999), Evaluating the use of “goodness-of-fit” Measures in hydrologic and hydroclimatic model validation, Water Resour. Res., 35(1), 233-241, doi:10.1029/1998WR900018.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{plot.ObjFct}}}
\examples{

obs <- 1:100 # 'observations'

# simulated and observed values agree
sim <- obs 
ObjFct(sim, obs)

# simulation has a bias
sim <- obs - 50
ObjFct(sim, obs)

# negative correlation
sim <- 100:1 
ObjFct(sim, obs)

# same mean, same correlation but smaller variance
sim <- 0.5 * obs + 25.25
ObjFct(sim, obs)

# small scatter around observations
sim <- obs * rnorm(100, 1, 0.1) 
ObjFct(sim, obs)

# larger scatter around observations
sim <- obs * rnorm(100, 1, 0.8) 
ObjFct(sim, obs)

# bias and larger scatter around observations
sim <- obs * rnorm(100, 2, 0.8) 
ObjFct(sim, obs)

# simulation is independent from observations
sim <- rnorm(100, 0, 1) 
ObjFct(sim, obs)

# split by groups
sim <- obs * c(rnorm(40, 1, 0.2), rnorm(60, 1, 2))
groups <- c(rep("subset 1", 40), rep("subset 2", 60))
of <- ObjFct(sim, obs, groups=groups)
of

# convert objective functions to text
text(of)
text(of, which="KGE")
text(of, which=c("R2", "MEF", "KGE"), sep=" ")

# plot scatterplot of two metrics
plot(of)
plot(of, which=c("MEF", "RMSE"))


# analyze relations between objective functions
#----------------------------------------------

# Some objective function metrics are closely related to others. 
# This simple example demonstrates the relations bewteen metrics.
# Therefore, several experiments are performed. In each experiment,
# simulation with different biases, correlations, and variance in
# comparison with the observations are created.
# Objective functions are then computed for all experiments.

# the 'observations'
obs <- 1:100

# experiments: create 'simulations' with different biases, correlations, and variances
n <- 500 # how many experiments?
data <- data.frame(obs=NA, sim=NA, exp=NA)
for (i in 1:n) {
   fbias <- runif(1, 0.01, 2) # factor to create the bais
   fcor <- runif(1, -2.5, 2.5) # factor to create a different correlation
   fsd <- runif(1, 0.1, 2) # factor to create variability
   sim <- fbias * obs^(fcor) # create simulations
   sim <- sim + rnorm(length(sim), 0, fsd*abs(mean(sim, na.rm=TRUE)))
   data <- rbind(data, data.frame(obs=obs, sim=sim, exp=i))
}
data <- na.omit(data)

# plot the first 5 experiments:
plot(sim ~ obs, data=data[1:500,], col=data$exp[1:500], pch=16)

# compute objective function metrics for each experiment (experiments as groups)
of <- ObjFct(data$sim, data$obs, groups=data$exp)
hist(of$Cor) # distribution of correlation across all experiments

# check relations between metrics
plot(of, c("Cor", "Spearman"))
plot(of, c("Cor", "R2"))
plot(of, c("Cor", "IoA"))
plot(of, c("RMSE", "AE"))
plot(of, c("RMSE", "MEF"))
plot(of, c("KS", "MEF"))

# check the correlation of metrics:
df <- t(laply(of, function(l) l)) # create a data.frame
colnames(df) <- attr(of, "names")
r <- cor(df) # compute correlation between metrics

# plot the correlation matrix
# red = positive correlation, blue = negative correlation
par(las=2)
brks <- seq(-1, 1, 0.05)
.fun <- colorRampPalette(c("blue", "white", "red"))
cols <- .fun(length(brks)-1)
image(r, axes=FALSE, breaks=brks, col=cols) 
box()
x <- seq(0, 1, length=nrow(r))
axis(1, at=x, rownames(r))
axis(2, at=x, rownames(r))


}
