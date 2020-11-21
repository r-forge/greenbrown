\name{TSGFstm}
\alias{TSGFstm}
\title{Temporal smoothing and gap filling based on a season-trend model}
\description{This function fills gaps in a time series by using a season-trend model as in \code{\link{TrendSTM}} (Verbesselt et al. 2010, 2012).}
\usage{TSGFstm(Yt, interpolate = FALSE, 
    ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values by using \code{\link{na.spline}}?}
  \item{\dots}{further arguments to \code{\link{TrendSTM}}.}
}

\value{The function returns a gap-filled and smoothed version of the time series.}
\references{Verbesselt, J.; Hyndman, R.; Zeileis, A.; Culvenor, D., Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment 2010, 114, 2970-2980. \cr
Verbesselt, J.; Zeileis, A.; Herold, M., Near real-time disturbance detection using satellite image time series. Remote Sensing of Environment 2012, 123, 98-108.}
\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TsPP}}, \code{\link{TrendSTM}} }
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFstm(gaps)
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled")
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

}
