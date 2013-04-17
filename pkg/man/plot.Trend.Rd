\name{plot.Trend}
\alias{plot.Trend}
\title{Create time series plots with trend lines}
\description{This is the standard plot function for results of the \code{\link{Trend}} function. See \code{\link{plot.default}} for further specifications of basic plots.}
\usage{\method{plot}{Trend}(x, ylab = "NDVI", ...)}
\arguments{
  \item{x}{Object of class 'Trend' as returned from function \code{\link{Trend}}}
  \item{ylab}{A title for the y axis}
  \item{\dots}{Further arguments that can be passed \code{\link{plot.default}}}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.default}}, \code{\link{plot.ts}}}
\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
summary(ndvi.trend)
plot(ndvi.trend)
	
}
