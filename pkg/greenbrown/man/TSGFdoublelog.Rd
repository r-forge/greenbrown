\name{TSGFdoublelog}
\alias{TSGFdoublelog}
\title{Temporal smoothing and gap filling using double logisitic functions}
\description{This function fills gaps and smoothes a time series by fitting for each year a double logisitic function. Two definitions for the shape of the double logistic function are available: 'Elmore' fits a function according to Elmore et al. (2012) and 'Beck' fits a according to Beck et al. (2006). If the time series has no \code{\link{Seasonality}}, double logistic fitting will not be performed but smoothing and interpolation will be done according to the selected \code{backup} algorithm.}
\usage{TSGFdoublelog(Yt, interpolate = FALSE, 
    method = c("Elmore", 
        "Beck"), backup = NULL, 
    check.seasonality = 1:3, 
    ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values by using the logistic fit function?}
  \item{method}{Which kind of double logistic curve should be used? 'Elmore' (Elmore et al. 2012) or 'Beck' (Beck et al. 2006).}
  \item{backup}{Which \code{backup} algorithm should be used for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another \code{method} can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"}
  \item{check.seasonality}{Which methods in \code{\link{Seasonality}} should indicate TRUE (i.e. time series has seasonality) in order to calculate phenology metrics? 1:3 = all methods should indicate seasonality, Set to NULL in order to not perform seasonality checks.}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a gap-filled and smoothed version of the time series.}
\references{Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334. \cr
Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674. }
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{FitDoubleLogBeck}}, \code{\link{FitDoubleLogElmore}}, \code{\link{TsPP}}, \code{\link{Phenology}}}
\examples{
# # introduce random gaps 
# gaps <- ndvi
# gaps[runif(100, 1, length(ndvi))] <- NA
# plot(gaps)
# 
# # do smoothing and gap filling
# tsgf <- TSGFdoublelog(gaps, method="Elmore")
# plot(gaps)
# lines(tsgf, col="red")

}
