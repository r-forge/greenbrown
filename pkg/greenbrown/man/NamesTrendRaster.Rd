\name{NamesTrendRaster}
\alias{NamesTrendRaster}
\title{Get the layer names for a TrendRaster raster brick}
\description{This function returns the layer names of a raster brick that was created using \code{\link{TrendRaster}}}
\usage{NamesTrendRaster(x)}
\arguments{
  \item{x}{\code{RasterBrick} as created with \code{\link{TrendRaster}} or \code{integer} as the maximum number of breakpoints that was used when the function \code{\link{TrendRaster}} was called.}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TrendRaster}}}
\examples{
# load a raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate trend with maximum 2 breakpoints
breaks <- 2
trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, 
   method="AAT", breaks=breaks)
plot(trendmap)

# layer names of the result
NamesTrendRaster(breaks)
NamesTrendRaster(trendmap)
names(trendmap)

# now imagine you are loosing the layer names ...
names(trendmap) <- 1:11
plot(trendmap)	# X1, X2 ... is not meaningfull. How can you get the names back?

# re-create the layer names with NamesTrendRaster
names(trendmap) <- NamesTrendRaster(trendmap)
plot(trendmap)

}
