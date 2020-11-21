\name{TrendLongestSEG}
\alias{TrendLongestSEG}
\title{Extract slope and p-value for the longest time series segment from a TrendRaster raster brick }
\description{This function extracts the slope and p-value of a trend for the longest time series segment from a raster brick that was created with \code{\link{TrendRaster}}}
\usage{TrendLongestSEG(r)}
\arguments{
  \item{r}{\code{RasterBrick} as created with \code{\link{TrendRaster}} or object of class 'Trend' as returned by \code{\link{Trend}}}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TrendRaster}}}
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate trend 
trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=1)
plot(trendmap)

# select trend and p-value only for the longest time series segment
trendmap.longestseg <- TrendLongestSEG(trendmap)
plot(trendmap.longestseg)

}
