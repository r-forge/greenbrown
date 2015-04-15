\name{plot.CompareClassification}
\alias{plot.CompareClassification}
\title{plot a comparison of two classification rasters}
\description{This function takes an object of class \code{\link{CompareClassification}} as input and plots a map of the class agreement of two classifications.}
\usage{\method{plot}{CompareClassification}(x,
 xlab = "", ylab = "", main = "Classification agreement", 
    names = NULL, ul = "burlywood4", lr = "darkgreen", ll = "khaki1", 
    ur = "royalblue1", ctr = "gray87", mar = NULL, ...)}\arguments{
  \item{x}{Object of class 'CompareClassification' as returned from function \code{\link{CompareClassification}}.}
  \item{xlab}{A title for the x axis}
  \item{ylab}{A title for the y axis}
  \item{main}{A title for the plot}
  \item{names}{a list with names of the two classifications and class names. See example section for details.}
  \item{ul}{starting color in the upper left corner of the \code{\link{ColorMatrix}}}
  \item{lr}{ending color in the lower right corner of the \code{\link{ColorMatrix}}}
  \item{ll}{starting color in the lower left corner of the \code{\link{ColorMatrix}}}
  \item{ur}{ending color in the upper right corner of the \code{\link{ColorMatrix}}	}
  \item{ctr}{color in the center of the \code{\link{ColorMatrix}}	}
  \item{mar}{plot margins}
  \item{\dots}{Further arguments that can be passed \code{\link{plot.default}}}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{CompareClassification}}, \code{\link{AccuracyAssessment}}, \code{\link{TrendClassification}}}
\examples{
# Example: calculate NDVI trends from two methods and compare the significant trends

# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)

# calculate trends with two different methods
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=0)
plot(AATmap)
STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=0)
plot(STMmap)

# classify the trend estimates from the two methods into significant positive, negative and no trend
AATmap.cl <- TrendClassification(AATmap)
plot(AATmap.cl, col=brgr.colors(3))
STMmap.cl <- TrendClassification(STMmap)
plot(STMmap.cl, col=brgr.colors(3))

# compare the two classifications
compare <- CompareClassification(x=AATmap.cl, y=STMmap.cl, names=list('AAT'=c("Br", "No", "Gr"), 'STM'=c("Br", "No", "Gr")))
compare

# plot the comparison
plot(compare)

}
