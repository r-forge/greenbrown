\name{MtsPlot}
\alias{MtsPlot}
\title{Multiple time series plot}
\description{Allows to plot multiple time series with different y-axes into one plot. This function is an alternative approach to plot trends than \code{\link{plot.Trend}}. It is especially suited to plot trends of different variables with different units in one figure.}
\usage{MtsPlot(x, xlim = NULL, 
    rge = c(0, 1), ylim = NULL, 
    col = "blue", lwd = 1, 
    add = FALSE, axis = TRUE, 
    ylab = "x", axis.pos = "l", 
    axis.col = col, axis.lim = NULL, 
    trend = TrendAAT, 
    trend.period = NULL, 
    trend.text = 1, unit = "", 
    text = "", unc = "polygon", 
    ensfun = mean, ...)}
\arguments{
  \item{x}{time series that should be plotted. If \code{x} has multiple columns, uncertainties can be plotted as individual 'lines' or as 'polygon' (see 'unc').}
  \item{xlim}{range of x-axis}
  \item{rge}{range of y-axis in which the time series should be plotted. }
  \item{ylim}{\code{axis} limits of the y-axis for the data in \code{x}}
  \item{col}{color of the time series and corresponding \code{axis}}
  \item{lwd}{line width of the time series}
  \item{add}{initialize a new plot if FALSE or adds time series to existing plot if TRUE}
  \item{axis}{draws a y-axis if TRUE}
  \item{ylab}{label for y-axis}
  \item{axis.pos}{position of y-axis: l (left) or r (right) }
  \item{axis.col}{color for the \code{axis}}
  \item{axis.lim}{limits (min/max) for the \code{axis}}
  \item{trend}{compute and adds a \code{trend} line using \code{\link{TrendText}}. Set to NULL for no trends.}
  \item{trend.period}{define a sub-period (e.g. c(1990, 2015)) to compute trends for the full time series and this period}
  \item{trend.text}{type of \code{trend} \code{text} to plot: 1 (\code{trend} in \%), 2 (\code{trend} slope with \code{unit}), 3 (\code{trend} slope without \code{unit}), 4 (p-value from Mann-Kendall \code{trend} test), 5 (only if \code{x} has multiple columns: p-value Wilcoxon rank sum test). In case of other values, no \code{text} will be plotted. In case of trend.text=5, the Wilcoxon rank sum test is used to test if the ensmble of \code{trend} slopes for each column in \code{x} is significant different from 0.}
  \item{unit}{\code{unit} of the values in \code{x} (used for \code{trend} \code{text})}
  \item{text}{additional \code{text} that should be plotted next to each time series}
  \item{unc}{if \code{x} has multiple columns, plot uncertainty ranges as 'polygon' or line'?}
  \item{ensfun}{if \code{x} has multiple columns, how to compute the main estimate of the emsemble?}
  \item{\dots}{further arguments to \code{\link{plot.default}}}
}
\details{This function plots a scatterplot.}
\value{A list with information about the estimated \code{trend}.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TrendText}}, \code{\link{plot.Trend}}}
\examples{

data(ndvi)
ndvi <- aggregate(ndvi, FUN=mean)

# default plot
MtsPlot(ndvi, ylab="NDVI")
MtsPlot(ndvi, ylab="NDVI", text="GIMMS")
MtsPlot(ndvi, rge=c(0.4, 1)) # plot time series in (0.4-1) range of y-axis
MtsPlot(ndvi, rge=c(0.5, 1), ylim=c(0,1)) # use (0-1) range for y-values and 
  # plot in (0.5-1) range of y-axis

# plot with uncertainty ranges
ndvi2 <- cbind(ndvi, ndvi * 1.3, ndvi * 0.7)
MtsPlot(ndvi2, rge=c(0.5, 1)) # uncertainty as polygon
MtsPlot(ndvi2, add=TRUE, rge=c(0, 0.5), unc="line", col="red") # uncertainty as line

# plot multiple time series
ndvi2 <- ndvi * 2 # scaled NDVI
ndvi3 <- ndvi^2 # squared NDVI
par(mar=c(3, 3, 2, 3))
MtsPlot(ndvi, rge=c(0.66, 1), ylab="NDVI")
MtsPlot(ndvi2, add=TRUE, rge=c(0.33, 0.66), axis.pos="r", ylab="NDVI*2", col="red")
MtsPlot(ndvi3, add=TRUE, rge=c(0, 0.33), axis.pos="l", ylab="NDVI^2", col="purple")

# options for trend
MtsPlot(ndvi, ylab="NDVI", trend=NULL) # no trend
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996)) # compute trend in subperiod
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=1) # text: trend in \% (default)
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=2) # text: trend in units
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=3) # text: trend without unit
MtsPlot(ndvi, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=4) # text: p-value only

ndvi2 <- cbind(ndvi, ndvi * 1.3, ndvi * 0.7)
MtsPlot(ndvi2, ylab="NDVI", trend.period=c(1982, 1996), 
        unit="NDVI", trend.text=5) # text: p-value only

}
