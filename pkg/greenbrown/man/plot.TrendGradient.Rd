\name{plot.TrendGradient}
\alias{plot.TrendGradient}
\title{Plotting function for objects of class TrendGradient}
\description{This function plots a gradient of trend slopes (e.g. latitudinal gradient). }
\usage{\method{plot}{TrendGradient}(x, 
    type = "xy", ylab = NULL, 
    xlab = NULL, col = "black", 
    ylim = NULL, xlim = NULL, 
    add = FALSE, symbolic = TRUE, 
    symbols = "standard", 
    ...)}
\arguments{
  \item{x}{Object of class 'TrendGradient' as returned from function \code{\link{TrendGradient}}}
  \item{type}{plotting \code{type}: 'xy' = gradient at \code{x} axis and slope at y axis, 'yx' = gradient at y axis and slope at \code{x} axis.}
  \item{ylab}{A title for the y axis}
  \item{xlab}{A title for the \code{x} axis}
  \item{col}{line colors	}
  \item{ylim}{limits for y axis}
  \item{xlim}{limits for \code{x} axis}
  \item{add}{\code{add} to exisiting plot?}
  \item{symbolic}{Add p-value as \code{symbols} (TRUE) or not (FALSE). If TRUE the p-value of a trend slope is added as symbol to the plot. }
  \item{symbols}{Type of \code{symbols} for p-values. "standard": *** (p <= 0.001), ** (p <= 0.01), * (p <= 0.05), . (p <= 0.1) and no symbol if p > 0.1.; "simple": * (p <= 0.05), \code{x} (p < 0.1)}
  \item{\dots}{Further arguments that can be passed \code{\link{plot.default}}}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{plot.default}}, \code{\link{plot.ts}}}
\examples{
# # compute a latitudinal gradient of trends (by default the method 'AAT' is used)
# gradient <- TrendGradient(ndvimap, start=c(1982, 1), freq=12)
# gradient
# plot(gradient) 
# # shown is the trend at each latitudinal band, the area represents the 95\% 
# # confidence interval of the trend (computed with function TrendUncertainty), 
# # symbols indicate the p-value of the trend at each latitude
# 
# plot(gradient, type="yx") # the gradient can be also plotted in reversed order
# 
# # compute gradients with different trend methods
# gradient.mac <- TrendGradient(ndvimap, start=c(1982, 1), freq=12, 
#    method="SeasonalAdjusted", funSeasonalCycle=MeanSeasonalCycle)
# plot(gradient.mac, col="blue", ylab="NDVI trend (month-1)")
# 
# # method AAT uses annual time steps, convert years -> months
# gradient$Slope <- gradient$Slope / 12 
# gradient$SlopeUncLower <- gradient$SlopeUncLower / 12
# gradient$SlopeUncUpper <- gradient$SlopeUncUpper / 12
# gradient$SlopeUncMedian <- gradient$SlopeUncMedian / 12
# plot(gradient, col="red", add=TRUE)
# 
# 
}
