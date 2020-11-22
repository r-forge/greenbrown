\name{brgr.colors}
\alias{brgr.colors}
\title{Brown-to-green color palette for NDVI trend maps}
\description{Positive trends in Normalized Difference Vegetation Index are called 'greening' whereas negative trends are called 'browning'. Creating maps of NDVI trends in these colors helps to read the map. This function provides a color scale from brown to green and can be used to plot NDVI trend maps.}
\usage{brgr.colors(n)}
\arguments{
  \item{n}{Number of color levels}
}

\value{A character vector of color names.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TrendRaster}}}
\examples{

cols <- brgr.colors(10)
brks <- seq(0, 1, 0.1)
plot(ndvimap, c(3, 6), col=cols, breaks=brks)

}
