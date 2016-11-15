\name{FitLogistic}
\alias{FitLogistic}
\title{Fit logistic functions }
\description{Fit additive or multiplicative logistic functions to a response variable.}
\usage{FitLogistic(x, y, additive = FALSE, method = c("genoud"), ntry = 50, 
    ...)}
\arguments{
  \item{x}{predictor variables}
  \item{y}{response variables}
  \item{additive}{additive or multiplicative (default) fit?}
  \item{method}{Method to be used for optimization of fit parameters. "genoud" uses genetic optimization (see \code{\link{genoud}}). Can be also one of "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent" (see \code{\link{optim}}). If two methods are provided, the second method is applied on the best result of the first method.}
  \item{ntry}{number of inital trials for strating parameter sets in the optimization of fit parameters. A higher number of trials yields usually more robust results but takes longer.}
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
of <- ObjFct(fit$predicted, y)
of
plot(of)

## 2D example
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
#plot(y, fit$predicted)
#fit$par # fitted parameter

## plot fit as a surface:
#surf <- expand.grid(x1=0:100, x2=0:100)
#surf <- data.frame(surf, y=predict(fit, surf))
#require(rgl)
#with(data.frame(x1, x2, y), plot3d(x1, x2, y))
#with(surf, surface3d(unique(x1), unique(x2), y, alpha=0.2, col="blue"))


}
