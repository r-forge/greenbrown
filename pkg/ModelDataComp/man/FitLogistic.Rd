\name{FitLogistic}
\alias{FitLogistic}
\title{Fit logistic functions }
\description{Fits logistic functions between one or several predictor variables and a response variable. In case of multi-variate fits, logistic functions can be combined either additatively or multiplicatively: \cr
Additive: y = Logistic(x1) + Logistic(x2) + Logistic(xN) \cr
Multiplicative: y = Logistic(x1) * Logistic(x2) * Logistic(xN) }
\usage{FitLogistic(x, y, additive = FALSE, method = c("genoud"), ninit = 50, 
    ...)}
\arguments{
  \item{x}{predictor variables}
  \item{y}{response variables}
  \item{additive}{Make an additive or multiplicative fit? The default is a multiplicative fit.}
  \item{method}{Method to be used for optimization of fit parameters. "genoud" uses genetic optimization (see \code{\link{genoud}}). Can be also one of "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent" (see \code{\link{optim}}). If two methods are provided, the second method is applied on the best result of the first method.}
  \item{ninit}{number of inital parameter sets for the optimization. Inital parameter sets will be included in genoud as part of the first generation. Each initial parameter set will be used as starting value within \code{\link{optim}} methods. A higher number of ninit will likely avoid solutions close to local optima but is computationally more expensive.}
  \item{\dots}{further arguments for \code{\link{genoud}} or \code{\link{optim}}}
}
\details{No details.}
\value{An object of class 'FitLogistic' which is actually a list.}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Logistic}}}
\examples{

# 1D example
x <- 1:1000
y <- Logistic(c(1, 0.01, 500), x) + rnorm(1000, 0, 0.01)
plot(x, y)
fit <- FitLogistic(x, y)
lines(x, fit$predicted, col="red")
fit$par # fitted parameter

# performance of fit
ScatterPlot(fit$predicted, y, objfct=TRUE)

## 2D example - takes more time
#n <- 1000
#x1 <- runif(n, 0, 100)
#x2 <- runif(n, 0, 100)
#f1 <- Logistic(c(1, 0.1, 50), x1)
#f2 <- Logistic(c(1, -1, 20), x2)
#plot(x1, f1)
#plot(x2, f2)
#y <- f1 * f2 * rnorm(n, 1, 0.1)
#plot(x1, y) 
#plot(x2, y)
#fit <- FitLogistic(x=cbind(x1, x2), y)
#ScatterPlot(fit$predicted, y, objfct=TRUE)

}
