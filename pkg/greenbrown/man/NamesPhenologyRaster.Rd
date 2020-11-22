\name{NamesPhenologyRaster}
\alias{NamesPhenologyRaster}
\title{Get the layer names for a PhenologyRaster raster brick}
\description{This function returns the layer names of a raster brick that was created using \code{\link{PhenologyRaster}}}
\usage{NamesPhenologyRaster(x, 
    start = NULL)}
\arguments{
  \item{x}{\code{RasterBrick} as created with \code{\link{PhenologyRaster}} or \code{integer} as the number of years of the input data when the function \code{\link{PhenologyRaster}} was called.}
  \item{start}{beginning of the time series.	}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{PhenologyRaster}}}

