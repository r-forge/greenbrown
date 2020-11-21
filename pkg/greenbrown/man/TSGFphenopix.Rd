\name{TSGFphenopix}
\alias{TSGFphenopix}
\title{Temporal smoothing and gap filling using phenopix}
\description{Time series smoothing and gap filling using fitting methods as provided in the \code{\link{greenProcess}} function of the \code{phenopix} package. Function fits are performed for each year separately for which \code{\link{PhenopixMY}} is used. }
\usage{TSGFphenopix(Yt, interpolate = FALSE, 
    fit = "spline", ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values?}
  \item{fit}{fitting function to be applied, available options are: spline, beck, elmore, klosterman, gu (see \code{\link{greenProcess}})}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a gap-filled and smoothed version of the time series.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{PhenopixMY}}, \code{\link{TsPP}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf <- TSGFphenopix(gaps, fit="spline")
plot(gaps)
lines(tsgf, col="red")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled")
abline(0,1)
r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
legend("topleft", paste("Cor =", round(r, 3)))

# compare spline from phenopix with TSGFspline
spl <- TSGFspline(gaps)
plot(gaps)
lines(tsgf, col="red")
lines(spl, col="blue")
legend("topleft", c("TSGFphenopix.spline", "TSGFspline"), text.col=c("red", "blue"))
# Note that the differences originate from the fact that TSGFspline is applied on 
# the full time series whereas spline within phenopix is applied for each year 
# separetely. Yearly fits for TSGFphenopix.spline are afterwards combined to a full 
# time series. This can cause jumps or peaks between two years. Thus, TSGFspline is 
# the better choice for multi-year time series. This is also seen in cross-validation:
plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled", col="red")
points(ndvi[is.na(gaps)], spl[is.na(gaps)], col="blue")
abline(0,1)
r <- cor(cbind(ndvi[is.na(gaps)], tsgf[is.na(gaps)], spl[is.na(gaps)]))
lgd <- paste(c("TSGFphenopix.spline", "TSGFspline"), "Cor =", round(r[1,2:3], 3))
legend("topleft", lgd, text.col=c("red", "blue"))

# Other fits wihtin phenopix might be usefull but are rather computationally expensive:
tsgf <- TSGFphenopix(gaps, fit="klosterman")
plot(gaps)
lines(tsgf, col="red")


}
