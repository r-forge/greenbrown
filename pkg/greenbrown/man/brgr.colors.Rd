\name{brgr.colors}
\alias{brgr.colors}
\title{Brown-to-green color palette for NDVI trend maps}
\description{Positive trends in Normalized Difference Vegetation Index are called 'greening' whereas negative trends are called 'browning'. Creating maps of NDVI trends in these colors helps to read the map. This function provides a color scale from brown to green and can be used to plot NDVI trend maps.}
\usage{brgr.colors(n)}
\arguments{
  \item{n}{Number of color levels}
}

\value{A character vector of color names.}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TrendRaster}}}
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)

# calculate trends and plot the result in nice brown-to-green colors
ndvitrend <- TrendRaster(ndvimap)
cols <- brgr.colors(15)
plot(ndvitrend, 2, col=cols, zlim=c(-0.004, 0.004))

classbreaks <- seq(-0.0035, 0.0035, by=0.001)
cols <- brgr.colors(length(classbreaks)-1)
plot(ndvitrend, 2, col=cols, breaks=classbreaks)

}
