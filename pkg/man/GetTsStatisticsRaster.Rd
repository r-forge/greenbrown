\name{GetTsStatisticsRaster}
\alias{GetTsStatisticsRaster}
\title{Estimate statistical properties of time series in a multi-temporal raster dataset}
\description{This function computes statistical properties of the time series in a multi-temporal raster dataset. It calls \code{\link{Decompose}} to decompose the time series of each grid cell of a raster brick into a trend, inter-annual variability, seasonal and short-term variability time series components. In a next step the mean, the trend slope, the range  and standard deviation of the inter-annual variability, the range of the seasonal cycle as well as the range and standard devaition of the short-term variability are calculated. }
\usage{GetTsStatisticsRaster(r, start = c(1982, 1), freq = 12)}
\arguments{
  \item{r}{object of class \code{\link[raster]{brick}} with multi-temporal data.}
  \item{start}{first time step, e.g. c(1982, 1) for January 1982. See \code{\link{ts}} for details.}
  \item{freq}{the number of observations per unit of time, e.g. 12 for monthly data or 24 for bi-monthly data. See \code{\link{ts}} for details.}
}

\value{The function returns a RasterBrick with 7 layers (mean, trend slope, range of IAV, standard deviation of inter-annual variabililty, range of seasonal cycle, range and standard deviaition of short-term variabililty. }

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Decompose}}}
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate time series statistics 
ndvimap.tsstat <- GetTsStatisticsRaster(ndvimap)
plot(ndvimap.tsstat)
}
