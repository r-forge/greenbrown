\name{plot.Trend}
\alias{plot.Trend}
\title{Create time series plots with trend lines}
\description{This is the standard plot function for results of the \code{\link{Trend}} function. See \code{\link{plot.default}} for further specifications of basic plots.}
\usage{\method{plot}{Trend}(x, ylab = "NDVI", add = FALSE, col = c("black", "blue", 
    "red"), lty = c(2, 1, 2), lwd = 1, symbolic = FALSE, ...)}
\arguments{
  \item{x}{Object of class 'Trend' as returned from function \code{\link{Trend}}}
  \item{ylab}{A title for the y axis}
  \item{add}{add to exisiting plot}
  \item{col}{colors for time series (1), trend line (2) and breakpoint}
  \item{lty}{line types for time series (1), trend line (2) and breakpoint}
  \item{lwd}{
}
  \item{symbolic}{add significance as symbols (TRUE) or as legend text (FALSE). If TRUE the p-value of a trend slope is added as symbol as following: *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1. }
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
plot(ndvi.trend)

ndvi.trend.aat <- Trend(ndvi, method="AAT", mosum.pval=1)
plot(ndvi.trend.aat)
plot(ndvi.trend.aat, symbolic=TRUE)

ndvi.trend.stm <- Trend(ndvi, method="STM", mosum.pval=1)
plot(ndvi.trend.stm, symbolic=TRUE)

plot(ndvi.trend.aat, symbolic=TRUE, ylim=c(0.23, 0.31), col=c("blue", "blue", "red"))
plot(ndvi.trend.stm, symbolic=TRUE, col=c("darkgreen", "darkgreen", "red"), lty=c(0, 1, 1), add=TRUE)
	
}
