\name{SplitRasterEqually}
\alias{SplitRasterEqually}
\title{Splits a raster in equal-area parts}
\description{This function splits a raster object in parts with ~ equal area. }
\usage{SplitRasterEqually(data.r, n)}\arguments{
  \item{data.r}{raster, raster brick or raster stack.}
  \item{n}{number of parts}
}

\value{the function returns a list of raster layers}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de> [aut, cre]}




\examples{
# data(ndvimap)
# tiles.l <- SplitRasterEqually(ndvimap, n=4)
# tiles.l
}
