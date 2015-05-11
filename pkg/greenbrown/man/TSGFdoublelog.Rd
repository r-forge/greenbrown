\name{TSGFdoublelog}
\alias{TSGFdoublelog}
\title{Temporal smoothing and gap filling using double logisitic functions}
\description{This function fills gaps and smoothes a time series by fitting for each year a double logisitic function. Two definitions for the shape of the double logistic function are available: 'Elmore' fits a function according to Elmore et al. (2012) and 'Beck' fits a according to Beck et al. (2006). If the time series has no Seasonality, double logistic fitting will not be performed but smoothing and interpolation will be done according to the selected \code{backup} algorithm.}
\usage{TSGFdoublelog(Yt, interpolate = FALSE, method = c("Elmore", "Beck"), 
    backup = NULL, ...)}\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values by using the logistic fit function?}
  \item{method}{Which kind of double logistic curve should be used? 'Elmore' (Elmore et al. 2012) or 'Beck' (Beck et al. 2006).}
  \item{backup}{Which backup algorithm should be used for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a gap-filled and smoothed version of the time series.}
\references{Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334. \cr
Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674. }
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de> [aut, cre]}



\seealso{\code{\link{FitDoubleLogBeck}}, \code{\link{FitDoubleLogElmore}}, \code{\link{TsPP}}, \code{\link{Phenology}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf1 <- TSGFdoublelog(gaps, method="Elmore")
tsgf2 <- TSGFdoublelog(gaps, method="Beck")
plot(gaps)
lines(tsgf1, col="red")
lines(tsgf2, col="blue")

# compare original data with gap-filled data
all <- ts.union(ndvi, tsgf1, tsgf2)
all[!is.na(gaps),] <- NA
plot(all[,1], all[,2], col="red", xlab="original", ylab="gap filled")
points(all[,1], all[,3], col="blue")
abline(0,1)
r <- c(cor(all[,1], all[,2], use="pairwise.complete.obs"), 
	cor(all[,1], all[,3], use="pairwise.complete.obs"))
legend("topleft", paste(c("Elmore Cor =", "Beck Cor ="), round(r, 3)), text.col=c("red", "blue"))

}
