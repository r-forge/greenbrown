ObjFct <- structure(function(
	##title<< 
	## Objective functions
	##description<<
	## Calculates several model performance metrics.

	sim,
	### simulated/predicted/modeled values
	
	obs,
	### observed/reference values
	
	groups=NULL
	### vector of groups to compute objective functions for several subsets 

	##details<<
	## The function computes several model performance metrics. These metrics are commonly used for the evaluation and optimization of environmental models (Janssen and Heuberger 1995, Legates et al. 1999, Krause et al. 2005, Gupta et al. 2009). The following metrics are implemented:
	## \itemize{ 
	## \item{ Pearson correlation coefficient and p-value: \code{\link{cor.test}} }
	## \item{ Spearman correlation coefficient and p-value: \code{\link{cor.test}} }
	## \item{ Slope of linear regression: \code{\link{lm}} }
	## \item{ Coefficient of determination: \code{\link{lm}} }
	## \item{ Metrics as described in Janssen and Heuberger (1995): }
	## \itemize{ 
	## \item{ Average error }
	## \item{ Normalized average error }
   ## \item{ Fractional mean bias }
	## \item{ Relative mean bias }
	## \item{ Fractional variance }
	## \item{ Variance ratio }
	## \item{ Kolmogorov-Smirnov statistic: \code{\link{ks.test}} }
	## \item{ Root mean squared error }
	## \item{ Normalized RMSE }
	## \item{ Index of agreement }
	## \item{ Mean absolute error }
	## \item{ Normalized mean absolute error }
	## \item{ Maximal absolute error }
	## \item{ Median absolute error }
	## \item{ Upper quartile absolute error }	
	## \item{ Ratio of scatter }
	## \item{ Modelling efficiency (Nash-Sutcliffe efficiency) }	
	## }
	## \item{ Percent bias }
	## \item{ Sum squared error }
	## \item{ Mean squared error }
   ## \item{ Kling-Gupta efficiency (Gupta et al. 2009): }
	## \itemize{ 
	## \item{ Kling-Gupta efficiency }
	## \item{ fractional contribution of bias }
	## \item{ fractional contribution of variance }
	## \item{ fractional contribution of correlation }
	## }
	## }

	##references<< 
	## Gupta, H. V., H. Kling, K. K. Yilmaz, and G. F. Martinez (2009), Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling, Journal of Hydrology, 377(1-2), 80-91, doi:10.1016/j.jhydrol.2009.08.003. \cr
   ## Janssen, P. H. M., and P. S. C. Heuberger (1995), Calibration of process-oriented models, Ecological Modelling, (83), 55-66. \cr
   ## Krause, P., D. P. Boyle, and F. Baese (2005), Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, doi:10.5194/adgeo-5-89-2005. \cr
   ## Legates, D. R., and G. J. McCabe (1999), Evaluating the use of "goodness-of-fit" Measures in hydrologic and hydroclimatic model validation, Water Resour. Res., 35(1), 233-241, doi:10.1029/1998WR900018.

	##seealso<<
	## \code{\link{plot.ObjFct}}, \code{\link{ObjFct2Text}}, \code{\link{WollMilchSauPlot}}

) { 
   sim <- as.vector(sim)
   obs <- as.vector(obs)
   if (length(sim) != length(obs)) stop("ObjFct: sim and obs need the same length.")
   if (is.null(groups)) {
      simobs <- na.omit(data.frame(sim, obs, rep(1, length(obs))))
      ngroups <- 0
      gr <- 1
   } else {
      simobs <- na.omit(data.frame(sim, obs, groups))
      gr <- sort(unique(simobs[,3]))
      ngroups <- length(gr)
   }
   
   obj.nms <- c("Cor", "Cor.pval", "Spearman", "Spearman.pval", "Slope", "R2", "AE", "NAE", "FB", "rB", "FV", "VR", "Pbias", "SSE", "MSE", "RMSE", "NRMSE", "IoA", "MAE", "NMAE", "MaxAE", "MedAE", "UpAE", "RS", "MEF", "KGE", "KGE.fBias", "KGE.fSd", "KGE.fCor", "KS", "KS.pval")
   obj.longnms <- c("Correlation coefficient", "Correlation p-value", "Spearman correlation", "Spearman p-value", "Regression slope", "Coefficient of determination", "Average error", "Normalized average error", "Fractional mean bias", "Relative mean bias", "Fractional variance", "Variance ratio", "Percent bias", "Sum squared error", "Mean squared error", "Root mean squared error", "Normalized RMSE", "Index of agreement","Mean absolute error", "Normalized mean absolute error", "Maximal absolute error", "Median absolute error", "Upper quartile absolute error", "Ratio of scatter", "Modelling efficiency", "Kling-Gupta efficiency", "KGE (bias fraction)", "KGE (variance fraction)", "KGE (correlation fraction)", "Kolmogorov-Smirnov statistic", "Kolmogorov-Smirnov p-value")
   
   obj.df <- data.frame(matrix(NA, nrow=ngroups+1, ncol=length(obj.nms)))

   for (g in 0:ngroups) {
      if (g == 0) bool <- rep(TRUE, nrow(simobs))
      if (g > 0) bool <- gr[g] == simobs[,3]
      sim <- simobs[bool,1]
      obs <- simobs[bool,2]
	   n <- length(sim)
		
	   # calculate residuals
	   res <- sim - obs
	   res2 <- res^2
	   res.abs <- abs(res)
	
	   # calculate inverse residuals
	   resin <- obs - sim
	   resin2 <- resin^2
	
	   # calculate statistics that depend on the number of layers
	   sim.mean <- mean(sim)
	   obs.mean <- mean(obs)	
	   sim.sd <- sd(sim)
	   obs.sd <- sd(obs)
	   sim.var <- sim.sd^2
	   obs.var <- obs.sd^2
	   sim.sum <- sum(sim)
	   obs.sum <- sum(obs)
	   res.sum <- sum(res)
	   resin.sum <- sum(resin)
	   resin2.sum <- sum(resin2)
		
	   # rank each values
	   obs.rank <- rank(obs)
	   sim.rank <- rank(sim)
	   sim.rank.mean <- mean(sim.rank)
	   obs.rank.mean <- mean(obs.rank)
		
	   # sum of squared error, mean squared error, mean absolute error, max absolute error, median absolute error, upper quartile absolute error
	   sse <- sum(res2)
	   mse <- mean(res2)
	   mae <- mean(res.abs) 
	   maxae <- max(res.abs)
	   medae <- quantile(res.abs, prob=0.5)
	   upae <- quantile(res.abs, prob=0.75)

	   # residuals from mean 
	   sim.res.mean <- sim - obs.mean
	   obs.res.mean <- obs - obs.mean
	
	   # calculate deviations
	   sim.dev <- sim - sim.mean
	   obs.dev <- obs - obs.mean

	   # calculate correlation coefficient
	   a <- sum(sim.dev * obs.dev)
	   b <- sqrt(sum(sim.dev^2)) * sqrt(sum(obs.dev^2))
	   r <- a / b
	
	   # p-value of correlation
	   suppressWarnings(r.p <- try(cor.test(sim, obs, method="pearson")$p.value, silent=TRUE))
	   if (class(r.p) == "try-error") r.p <- NA
	
	   # calculate spearman correlation
	   a <- (sim.rank - sim.rank.mean) * (obs.rank - obs.rank.mean)
	   b <- sum((sim.rank - sim.rank.mean)^2)
	   c <- sum((obs.rank - obs.rank.mean)^2)
	   d <- sqrt(b * c)
	   spearman <- sum(a) / d
	
	   # p-value of spearman
	   suppressWarnings(spearman.p <- try(cor.test(sim, obs, method="spearman")$p.value, silent=TRUE))
	   if (class(spearman.p) == "try-error") spearman.p <- NA
	
	   # regression slope
	   suppressWarnings(m <- try(lm(obs ~ sim), silent=TRUE))
	   sl <- coefficients(m)[2]
	   suppressWarnings(r2 <- summary(m)$r.squared)
	
	   # average error
	   ae <- sim.mean - obs.mean
	
	   # norm average error
	   nae <- ae / obs.mean
	
	   # fractional mean bias
	   fb <- ae / (0.5 * (sim.mean + obs.mean))
	
	   # relative mean bias
	   rb <- ae / obs.sd
	
	   # fractional variance
	   fv <- (sim.var - obs.var) / (0.5 * (sim.var + obs.var))
	
	   # variance ratio
	   vr <- sim.var / obs.var
	
	   # percent bias
	   pbias <- 100 * res.sum / obs.sum 
	
	   # RMSE
	   rmse <- sqrt(mse)
	
	   # normalized RMSE
	   nrmse <- rmse / obs.mean
	
	   # index of agreement
	   ioa <- 1 - resin2.sum / sum( ( abs(sim - obs.mean) + abs(obs - obs.mean))^2 ) 
	
	   # normalized mean absolute error
	   nmae <- mae / obs.mean
	
	   # ratio of scatter
	   a <- sum(obs.dev^2)
	   b <- sum(sim.res.mean^2)
	   rs <- a / b
	
	   # modelling efficiency
	   a <- sum(obs.res.mean^2)
	   mef <- 1 - resin2.sum / a
	
	   # Kling-Gupta efficiency with components
	   kge.beta <- (sim.mean / obs.mean - 1)^2
	   kge.alpha <- (sim.sd / obs.sd - 1)^2
	   if (is.na(kge.alpha)) {
	      warnings("Standard deviation of observations is 0. KGE cannot be computed exactely.")
	      kge.alpha <- (sim.sd / 0.0000000001 - 1)^2
	   }
	   rkge <- r
	   rkge[is.na(rkge)] <- 0 # if cor is NA (happens in case of constant sim or obs), set cor for KGE computation to 0 to get maximum error for correlation component
	   kge.r <- (rkge - 1)^2
	   kge.sum <- kge.r + kge.alpha + kge.beta
	   kge.fbeta <- kge.beta / kge.sum
	   kge.falpha <- kge.alpha / kge.sum
	   kge.fr <- kge.r / kge.sum
	   kge <- 1 - sqrt(kge.sum)
	   
	   # KS-Test
	   suppressWarnings(ks <- ks.test(sim, obs))
	
      # return all objective functions
	   obj <- c(r, r.p, spearman, spearman.p, sl, r2, ae, nae, fb, rb, fv, vr, pbias, sse, mse, rmse, nrmse, ioa, mae, nmae, maxae, medae, upae, rs, mef, kge, kge.fbeta, kge.falpha, kge.fr, ks$statistic, ks$p.value)
	   obj[is.infinite(obj) | is.nan(obj)] <- NA
	   obj.df[g+1, ] <- obj
	}

	colnames(obj.df) <- obj.nms
	if (ngroups > 0) attr(obj.df, "groupnames") <- c("all", as.character(gr))
	if (ngroups == 0) attr(obj.df, "groupnames") <- "all"
	attr(obj.df, "names") <- obj.nms
	attr(obj.df, "longnames") <- obj.longnms
	class(obj.df) <- c("ObjFct", "data.frame")
   return(obj.df)
   ### An object of class "ObjFct" which is actually a list.
}, ex=function() {

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
ScatterPlot(obs, sim, objfct=TRUE)

# simulation is independent from observations
sim <- rnorm(100, 0, 1) 
ObjFct(sim, obs)

# split by groups
sim <- obs * c(rnorm(40, 1, 0.2), rnorm(60, 1.2, 0.5))
groups <- c(rep("subset 1", 40), rep("subset 2", 60))
of <- ObjFct(sim, obs, groups=groups)
of
ScatterPlot(obs, sim, groups=groups, objfct=TRUE)

# convert objective functions to text
ObjFct2Text(of)
ObjFct2Text(of, which="KGE")
ObjFct2Text(of, which=c("R2", "MEF", "KGE"), sep=" ")

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

# experiments: create several 'simulations' 
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

# compute objective function metrics for each experiment
of <- ObjFct(data$sim, data$obs, groups=data$exp)
hist(of$Cor) 

# check relations between metrics
plot(of, c("Cor", "Spearman"))
plot(of, c("Cor", "R2"))
plot(of, c("Cor", "IoA"))
plot(of, c("RMSE", "AE"))
plot(of, c("RMSE", "MEF"))
plot(of, c("KS", "MEF"))

})


print.ObjFct <- structure(function(
	##title<< 
	## Print objective functions
	##description<<
	## Prints objective functions
	
	x,
	### an object of class \code{\link{ObjFct}}
	
	...
	### further arguments passed to or from other methods
	
	##details<<
	## Prints an object of class \code{\link{ObjFct}}.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{ObjFct}}
) {
      if (length(x$Cor) > 1) cat("                                 (group)", format(attr(x, "groupnames"), trim=TRUE, width=8, justify="right"), "\n")
      cat("Correlation-based metrics:", "\n")
		cat("  Correlation coefficient         Cor = ", format(x$Cor, digits=3, trim=TRUE, width=8), "\n")
		cat("  Correlation p-value        Cor.pval = ", format(x$Cor.pval, digits=3, trim=TRUE, width=8), "\n")
		cat("  Spearman correlation       Spearman = ", format(x$Spearman, digits=3, trim=TRUE, width=8), "\n")
		cat("  Spearman p-value      Spearman.pval = ", format(x$Spearman.pval, digits=3, trim=TRUE, width=8), "\n")
		cat("  Regression slope              Slope = ", format(x$Slope, digits=3, trim=TRUE, width=8), "\n")
		cat("  Coefficient of determination     R2 = ", format(x$R2, digits=3, trim=TRUE, width=8), "\n")
		cat("Bias-based metrics:", "\n")
		cat("  Average error                    AE = ", format(x$AE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Normalized average error        NAE = ", format(x$NAE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Fractional mean bias             FB = ", format(x$FB, digits=3, trim=TRUE, width=8), "\n")
		cat("  Relative mean bias               rB = ", format(x$rB, digits=3, trim=TRUE, width=8), "\n")
		cat("  Percent bias                  Pbias = ", format(x$Pbias, digits=3, trim=TRUE, width=8), "\n")
		cat("Variance-based metrics:", "\n")
		cat("  Fractional variance              FV = ", format(x$FV, digits=3, trim=TRUE, width=8), "\n")
		cat("  Variance ratio                   VR = ", format(x$VR, digits=3, trim=TRUE, width=8), "\n")
		cat("Squared error metrics:", "\n")
		cat("  Sum squared error               SSE = ", format(x$SSE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Mean squared error              MSE = ", format(x$MSE, digits=3, trim=TRUE, width=8), "\n")	
		cat("  Root mean squared error        RMSE = ", format(x$RMSE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Normalized RMSE               NRMSE = ", format(x$NRMSE, digits=3, trim=TRUE, width=8), "\n")
		cat("Absolute error metrics:", "\n")
		cat("  Mean absolute error             MAE = ", format(x$MAE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Normalized mean absolute error NMAE = ", format(x$NMAE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Median absolute error         MedAE = ", format(x$MedAE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Upper quartile absolute error  UpAE = ", format(x$UpAE, digits=3, trim=TRUE, width=8), "\n")
		cat("  Maximal absolute error        MaxAE = ", format(x$MaxAE, digits=3, trim=TRUE, width=8), "\n")
		cat("Distribution-based metrics:", "\n")
		cat("  Ratio of scatter                 RS = ", format(x$RS, digits=3, trim=TRUE, width=8), "\n")
		cat("  Kolmogorov-Smirnov statistic     KS = ", format(x$KS, digits=3, trim=TRUE, width=8), "\n")
		cat("  Kolmogorov-Smirnov p-value  KS.pval = ", format(x$KS.pval, digits=3, trim=TRUE, width=8), "\n")
		cat("Efficiency metrics:", "\n")		
		cat("  Index of agreement              IoA = ", format(x$IoA, digits=3, trim=TRUE, width=8), "\n")
      cat("  Modelling/Nash-Sutcliffe eff.   MEF = ", format(x$MEF, digits=3, trim=TRUE, width=8), "\n")
		cat("  Kling-Gupta efficiency          KGE = ", format(x$KGE, digits=3, trim=TRUE, width=8), "\n")
		cat("  KGE (bias fraction)       KGE.fBias = ", format(x$KGE.fBias, digits=3, trim=TRUE, width=8), "\n")
		cat("  KGE (variance fraction)     KGE.fSd = ", format(x$KGE.fSd, digits=3, trim=TRUE, width=8), "\n")
		cat("  KGE (correlation fraction) KGE.fCor = ", format(x$KGE.fCor, digits=3, trim=TRUE, width=8), "\n")      

})


ObjFct2Text <- structure(function(
	##title<< 
	## Convert objective functions to text
	##description<<
	## Converts an object of class \code{\link{ObjFct}} to text string.
	
	x,
	### an object of class \code{\link{ObjFct}}
	
	which=c("Cor", "RMSE", "MEF"), 
	### which objective function metrics should be written as text?
	
	sep=", ", 
	### separation between metrics
	
	digits=3
	### digits for rounding
	
	##details<<
	## Converts an object of class 'ObjFct' to text string.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{ObjFct}}
) {

	for (i in 1:length(which)) {
		txt.i <- paste(which[i], "=", format((x[[which[i]]]), digits=digits))
		if (i == 1) {	
			txt <- txt.i
		} else {
			txt <- paste(txt, sep, txt.i, sep="")
		}
	}
	return(txt)
}, ex=function() {

obs <- rnorm(100) # "observations"
sim <- obs + rnorm(100, 0, 0.8) # simulation
of <- ObjFct(sim, obs)
of

# convert objective functions to text:
ObjFct2Text(of)
ObjFct2Text(of, which="KGE")
ObjFct2Text(of, which=c("R2", "MEF", "KGE"), sep=" ")

})







