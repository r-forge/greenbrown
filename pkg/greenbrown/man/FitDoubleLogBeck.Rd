\name{FitDoubleLogBeck}
\alias{FitDoubleLogBeck}
\title{Fit a double logisitic function to a vector according to Beck et al. (2006)}
\description{This function fits a double logistic curve to observed values using the function as described in Beck et al. (2006) (equation 3).}
\usage{FitDoubleLogBeck(x, t = 1:length(x), tout = t, weighting = TRUE, 
    return.par = FALSE, plot = FALSE, ...)}\arguments{
  \item{x}{vector or time series to fit}
  \item{t}{time steps}
  \item{tout}{time steps of output (can be used for interpolation)}
  \item{weighting}{apply the weighting scheme to the observed values as described in Beck et al. 2006? This is useful for NDVI observations because higher values will get an higher weight in the estimation of the double logisitic function than lower values.	}
  \item{return.par}{if TRUE the function returns parameters of the double logisitic fit, if FALSE it returns the fitted curve}
  \item{plot}{plot iterations for logistic fit?}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a vector with fitted values if return.par is FALSE and it returns parameters of the logisitic modle of return.par is TRUE }
\references{Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TSGFdoublelog}}, \code{\link{Phenology}} }
\examples{

# load NDVI data
data(ndvi)
plot(ndvi)

# select one year of data
x <- as.vector(window(ndvi, start=c(1996,1), end=c(1996, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogBeck(x)
plot(x)
lines(fit, col="blue")

# return parameters of fit and plot iterations
FitDoubleLogBeck(x, return.par=TRUE, plot=TRUE)	# plot=TRUE causes plotting of different double logisitic

# fit double-logistic function to one year of data, interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogBeck(x, tout=tout)
PhenoDeriv(fit, plot=TRUE)

}
