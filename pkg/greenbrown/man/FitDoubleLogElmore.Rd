\name{FitDoubleLogElmore}
\alias{FitDoubleLogElmore}
\title{Fit a double logisitic function to a vector according to Elmore et al. (2012)}
\description{This function fits a double logistic curve to observed values using the function as described in Elmore et al. (2012) (equation 4).}
\usage{FitDoubleLogElmore(x, t = 1:length(x), tout = t, return.par = FALSE, 
    plot = FALSE, ...)}
\arguments{
  \item{x}{vector or time series to fit}
  \item{t}{time steps}
  \item{tout}{time steps of output (can be used for interpolation)}
  \item{return.par}{if TRUE the function returns parameters of the double logisitic fit, if FALSE it returns the fitted curve}
  \item{plot}{plot iterations for logistic fit?}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a vector with fitted values if return.par is FALSE and it returns parameters of the logisitic modle of return.par is TRUE}
\references{Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TSGFdoublelog}}, \code{\link{Phenology}} }
\examples{

# load NDVI data
data(ndvi)
plot(ndvi)

# select one year of data
x <- as.vector(window(ndvi, start=c(1997,1), end=c(1997, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
plot(x)
lines(fit, col="blue")

# return parameters of fit
FitDoubleLogElmore(x, return.par=TRUE, plot=TRUE)	# plot=TRUE causes plotting of different double logisitic

# fit double-logistic function to one year of data, interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout=tout)
plot(x)
lines(tout, fit, col="blue")
PhenoDeriv(fit, plot=TRUE)


}
