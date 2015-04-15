\name{TrendClassification}
\alias{TrendClassification}
\title{Classify a raster in greening and browning trends}
\description{This function classifies a RasterBrick with trend estimates as computed with \code{\link{TrendRaster}} into positive, negative and no trend per each time series segment.}
\usage{TrendClassification(r, min.length = 0, max.pval = 0.05, ...)}\arguments{
  \item{r}{multi-layer raster object of class \code{\link[raster]{brick}} as computed with \code{\link{TrendRaster}}}
  \item{min.length}{Minimum duration of a trend in time steps of the input raster (see Details).}
  \item{max.pval}{Maximum p-value to classify a trend as being significant.}
  \item{\dots}{additional arguments as for \code{\link{writeRaster}}}
}
\details{This function expects a RasterBrick as created with \code{\link{TrendRaster}} as input and classifies for each pixel and each time series segment if a trend is significant positive, significant negative or not significant (no trend). Per default a p-value of 0.05 is used to classify trends as significant. Additionally, the minimum duration of a trend can be specified with min.length: For example, if only time series segments longer than 10 years should be considered as trend, set min.length=11 in case of annual data. In case of monthly data set it to 132 (12 observations per year * 11 years). The function \code{\link{CompareClassification}} can be used to compare classified trends from different methods or data sets. }
\value{The function returns a RasterLayer in case of one time series segment or a RasterBrick in case of multiple time series segments. Pixels with a significant positive trend have the value 1; pixels with significant negative trends -1 and non-significant trends 0.}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TrendRaster}}, \code{\link{CompareClassification}}}
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
ndvimap
plot(ndvimap, 8)

# calculate trends on the raster
trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=2)
plot(trendmap)

# classify the trends in greening and browning
greenbrownmap <- TrendClassification(trendmap, min.length=10, max.pval=0.05)
plot(greenbrownmap, col=brgr.colors(3))

}
