\name{FitDoubleLogBeck}
\alias{FitDoubleLogBeck}
\title{Fit a double logisitic function to a vector according to Beck et al. (2006)}
\description{This function fits a double logistic curve to observed values using the function as described in Beck et al. (2006) (equation 3).}
\usage{FitDoubleLogBeck(x, t = 1:length(x), 
    tout = t, weighting = TRUE, 
    hessian = FALSE, 
    plot = FALSE, ninit = 30, 
    ...)}
\arguments{
  \item{x}{vector or time series to fit}
  \item{t}{time steps}
  \item{tout}{time steps of output (can be used for interpolation)}
  \item{weighting}{apply the \code{weighting} scheme to the observed values as described in Beck et al. 2006? This is useful for NDVI observations because higher values will get an higher weight in the estimation of the double logisitic function than lower values.	}
  \item{hessian}{compute standard errors of parameters based on the Hessian?}
  \item{plot}{\code{plot} iterations for logistic fit?}
  \item{ninit}{number of inital parameter sets from which to start optimization}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a list with fitted values, parameters, fitting formula and standard errors if \code{hessian} is TRUE}
\references{Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TSGFdoublelog}}, \code{\link{Phenology}} }
\examples{

# select one year of data
x <- as.vector(window(ndvi, start=c(1994,1), end=c(1994, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogBeck(x)
lines(fit$predicted, col="blue")

# # do more inital trials, plot iterations and compute parameter uncertainties
# FitDoubleLogBeck(x, hessian=TRUE, plot=TRUE, ninit=100)	
# 
# # fit double-logistic function to one year of data, 
# # interpolate to daily time steps and calculate phenology metrics
# tout <- seq(1, 12, length=365)	# time steps for output (daily)
# fit <- FitDoubleLogBeck(x, tout=tout)
# PhenoDeriv(fit$predicted, plot=TRUE)

}
