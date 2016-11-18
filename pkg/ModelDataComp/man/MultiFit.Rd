\name{MultiFit}
\alias{MultiFit}
\title{Multiple fits}
\description{Fits bivariate or multivariate regressions between a response variable and one or several predictor variables based on multiple fitting methods. }
\usage{MultiFit(x, y, fits = c("lm", "quantreg", "poly2", "poly3", "spline", 
    "gam"), xout = NULL, excl.quantile = c(0, 1), fit.quantile = NULL, 
    ...)}
\arguments{
  \item{x}{predictor variables: a vector for bivariate fits, or a matrix or data.frame for multivariate fits}
  \item{y}{vector of a response variable}
  \item{fits}{One or several fitting methods that should be used, possible options are: lm, quantreg, poly2, poly3, spline, gam, rf, logistic}
  \item{xout}{vector or data.frame of predictor variables for which fits should be returned. If NULL, fits are returned along a sequence of x values. This allows the plotting of 2D surfaces in case of two predictor variables (see examples). In case of xout=x, fits are returned for the same x values that were used for fitting.}
  \item{excl.quantile}{lower and upper quantiles for which x and y values should be excluded to compute fits. For example, if excl.quantile=c(0, 0.9) all x and y values above the quantile 0.9 will be excluded from fitting.}
  \item{fit.quantile}{Perform a fitting to a certain quantile of x? Setting this argument to an value between 0 and 1 allows quantile regression. Therfore \code{\link{SelectQuantiles}} is first used to select along a range of x only the values that are around the specified quantile.}
  \item{\dots}{further arguments (not used)}
}
\details{The following fitting methods are implemented:
\itemize{ 
\item{ "lm": (multiple) linear regression based on \code{\link{lm}}: lm(y ~ x) }
\item{ "quantreg": quantile regression to the median based on \code{\link{rq}}: rq(y ~ x, tau=0.5) }
\item{ "poly2": 2nd-order polynomial regression based on \code{\link{lm}}: lm(y ~ poly(x, degree=2)) }
\item{ "poly3": 3rd-order polynomial regression based on \code{\link{lm}}: lm(y ~ poly(x, degre=3)) }
\item{ "spline": smoothing spline based on \code{\link{smooth.spline}}: smooth.spline(x, y). This method only works for bivariate fits. }
\item{ "gam": generalized additive models using spline smoothing based on \code{\link{gam}}: gam(y ~ s(x)) }
\item{ "rf": random forest based on \code{\link{randomForest}}: randomForest(y ~ x). This method is not computed by default because it can be computationally expensive. }
\item{ "logistic": multiplicative logistic functions based on \code{\link{FitLogistic}}: FitLogistic(x, y). This method is not computed by default because it can be computationally expensive. }
} 
Furthermore, ensemble statistics like the mean, median, standard deviation and percentiles are computed from the results of the choosen fitting methods. }

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{FitLogistic}}}
\examples{

# bivariate example
x <- runif(1000, -3, 3) # predictor variable
y <- 0.5 * x + 1 / exp(-0.4 * x) * rnorm(1000, 1, 1) # response variable
ScatterPlot(x, y)
fit <- MultiFit(x, y, fits=c("lm", "quantreg", "poly2", "poly3", 
   "spline", "gam", "rf", "logistic"))
summary(fit)
cols <- piratepal("basel")
matplot(fit$x, fit[,2:11], type="l", add=TRUE, lty=1, col=cols, lwd=2)
legend("topleft", colnames(fit)[2:11], lty=1, col=cols, lwd=2)

# same example but exclude very high values (> quantile 0.9) from fitting
fit1 <- MultiFit(x, y, excl.quantile=c(0, 0.9))
lines(fit1$x, fit1$ensMean, type="l",lty=1, col="purple", lwd=3)

# to compare fitted with original values compute 
# fits at original predictor variables (xout=x)
fit <- MultiFit(x, y, fits=c("poly3", "gam"), xout=x) 
df <- data.frame(sim=c(fit$poly3, fit$gam), obs=rep(y, 2), groups=rep(c("poly3", "gam"), each=length(y)))
of <- ObjFct(df$sim, df$obs, df$groups)
plot(of, which="RMSE")
ScatterPlot(df$sim, df$obs, df$groups, objfct=TRUE)
TaylorPlot(df$sim, df$obs, df$groups)

# bivariate example with fit to a certain quantile
ScatterPlot(x, y)
fit <- MultiFit(x, y, fit.quantile=0.9, fits=c("spline", "gam", "poly3", "rf"))
matplot(fit$x, fit[,2:5], type="l", add=TRUE, lty=1, col=cols, lwd=2)
legend("topleft", colnames(fit)[2:5], lty=1, col=cols, lwd=2)

# example with two predictor variables
a <- runif(1000, -3, 3) # 1st predictor variable
b <- runif(1000, 0, 2) # 2nd predictor variable
y <- 1.2 * b + 1 / exp(-0.4 * a) * rnorm(1000, 1, 0.2) # response variable
plot(a, y)
plot(b, y)
fit <- MultiFit(x=data.frame(a, b), y, xout=NULL) 
image(x=unique(fit$a), y=unique(fit$b), 
   z=matrix(fit$lm, sqrt(nrow(fit))), main="ensMean")

## as 3D plot:
#require(rgl)
#with(data.frame(a, b), plot3d(a, b, y))
#with(fit, surface3d(unique(a), unique(b), ensMean, alpha=0.2, col="red"))

# example with three predictor variables
a <- runif(1000, -3, 3) # 1st predictor variable
b <- runif(1000, 0, 2) # 2nd predictor variable
c <- rnorm(1000, 1, 1) # 3rd predictor variable
y <- 1.2 * b + 1 / exp(-0.4 * a) * c # response variable
x <- data.frame(a, b, c)
fit <- MultiFit(x, y, fits=c("poly2", "rf"), xout=x)
ObjFct(fit$rf, y)
ObjFct(fit$poly2, y)


}
