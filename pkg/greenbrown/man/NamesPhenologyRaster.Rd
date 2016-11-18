\name{NamesPhenologyRaster}
\alias{NamesPhenologyRaster}
\title{Get the layer names for a PhenologyRaster raster brick}
\description{This function returns the layer names of a raster brick that was created using \code{\link{PhenologyRaster}}}
\usage{NamesPhenologyRaster(x, start = NULL)}
\arguments{
  \item{x}{\code{RasterBrick} as created with \code{\link{PhenologyRaster}} or \code{integer} as the number of years of the input data when the function \code{\link{PhenologyRaster}} was called.}
  \item{start}{beginning of the time series.	}
}



\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{PhenologyRaster}}}
\examples{
# # load a multi-temporal raster dataset of Normalized Difference Vegetation Index
# data(ndvimap)
# plot(ndvimap, 8)

# # calculate phenology
# phenmap <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12)
# plot(phenmap)
# plot(phenmap)

# # layer names of the result
# NamesPhenologyRaster(30)
# NamesPhenologyRaster(phenmap)
# NamesPhenologyRaster(phenmap, start=1982)
# names(phenmap)


}
