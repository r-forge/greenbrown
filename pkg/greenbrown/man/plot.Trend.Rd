\name{plot.Trend}
\alias{plot.Trend}
\title{Plot trend and breakpoint results}
\description{This is the standard plot function for results of the \code{\link{Trend}} function. See \code{\link{plot.default}} for further specifications of basic plots.}
\usage{\method{plot}{Trend}(x, ylab = "NDVI", add = FALSE, col = c("black", "blue", 
    "red", "blue"), lty = c(2, 1, 2, 3), lwd = 1, symbolic = TRUE, 
    legend = FALSE, uncertainty = TRUE, axes = TRUE, ...)}\arguments{
  \item{x}{Object of class 'Trend' as returned from function \code{\link{Trend}}}
  \item{ylab}{A title for the y axis}
  \item{add}{add to exisiting plot}
  \item{col}{colors for (1) time series, (2) trend line, (3) breakpoints and (4) trend uncertainty}
  \item{lty}{line types for (1) time series, (2) trend line, (3) breakpoints and (4) trend uncertainty}
  \item{lwd}{
}
  \item{symbolic}{add significance as symbols (TRUE). If TRUE the p-value of a trend slope is added as symbol as following: *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1. }
  \item{legend}{add slope and p-value as legend}
  \item{uncertainty}{plot uncertainty in trend slopes? (only possible if the x 'Trend' object includes uncertainty estimates)}
  \item{axes}{plot axes?}
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
plot(ndvi.trend, uncertainty=FALSE)

ndvi.trend.aat <- Trend(ndvi, method="AAT", mosum.pval=1)
plot(ndvi.trend.aat)
plot(ndvi.trend.aat, symbolic=FALSE)
plot(ndvi.trend.aat, symbolic=FALSE, uncertainty=FALSE)

ndvi.trend.stm <- Trend(ndvi, method="STM", mosum.pval=1)
plot(ndvi.trend.stm)

plot(ndvi.trend.aat, symbolic=TRUE, ylim=c(0.23, 0.31), col=c("blue", "blue", "red"))
plot(ndvi.trend.stm, symbolic=TRUE, col=c("darkgreen", "darkgreen", "red"), lty=c(0, 1, 1), add=TRUE)
	
}
