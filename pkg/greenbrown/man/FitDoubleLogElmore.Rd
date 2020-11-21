\name{FitDoubleLogElmore}
\alias{FitDoubleLogElmore}
\title{Fit a double logisitic function to a vector according to Elmore et al. (2012)}
\description{This function fits a double logistic curve to observed values using the function as described in Elmore et al. (2012) (equation 4).}
\usage{FitDoubleLogElmore(x, 
    t = 1:length(x), 
    tout = t, hessian = FALSE, 
    plot = FALSE, ninit = 100, 
    ...)}
\arguments{
  \item{x}{vector or time series to fit}
  \item{t}{time steps}
  \item{tout}{time steps of output (can be used for interpolation)}
  \item{hessian}{compute standard errors of parameters based on the Hessian?}
  \item{plot}{\code{plot} iterations for logistic fit?}
  \item{ninit}{number of inital parameter sets from which to start optimization}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a list with fitted values, parameters, fitting formula and standard errors if \code{hessian} is TRUE}
\references{Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TSGFdoublelog}}, \code{\link{Phenology}} }
\examples{

# select one year of NDVi data
data(ndvi)
x <- as.vector(window(ndvi, start=c(1991,1), end=c(1991, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
fit
plot(x)
lines(fit$predicted, col="blue")

# do more inital trials, plot iterations and compute parameter uncertainties
FitDoubleLogElmore(x, hessian=TRUE, plot=TRUE, ninit=1000)	

# fit double-logistic function to one year of data, 
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout=tout)
plot(x)
lines(tout, fit$predicted, col="blue")
PhenoDeriv(fit$predicted, plot=TRUE)


}
