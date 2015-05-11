\name{TSGFspline}
\alias{TSGFspline}
\title{Temporal smoothing and gap filling using splines}
\description{This function fills gaps in a time series by using \code{\link{na.spline}} and smoothes the time series by using  \code{\link{smooth.spline}}}
\usage{TSGFspline(Yt, interpolate = FALSE, ...)}\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values by using \code{\link{na.spline}}?}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a gap-filled and smoothed version of the time series.}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de> [aut, cre]}



\seealso{\code{\link{TsPP}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFspline(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled")
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

}
