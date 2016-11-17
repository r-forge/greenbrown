\name{ScatterPlot}
\alias{ScatterPlot}
\title{Enhanced scatterplot with fitting lines}
\description{This function creates a scatterplot with fitting lines. Points and fitting lines can be also computed by groups.}
\usage{ScatterPlot(x, y, groups = NULL, plot.type = NULL, objfct = FALSE, 
    which.objfct = c("Cor", "Pbias", "RMSE"), add = FALSE, density.levels = 50, 
    density.labels = FALSE, image.nbrks = 300, fits = c("spline"), 
    fit.quantile = NULL, fit.minnobs = 10, fit.groups = TRUE, 
    fit.global = TRUE, col.points = "black", col.image = NULL, 
    col.density = NULL, col.fit = col.points, col.global = NULL, 
    xlab = "", ylab = "", main = "", xlim = NULL, ylim = NULL, 
    lwd = 2, lty = 1, quantile.x = c(0.01, 0.99), quantile.y = c(0.01, 
        0.99), plot = TRUE, cex = NULL, pch = 21, alpha = NULL, 
    ...)}
\arguments{
  \item{x}{vector of x values}
  \item{y}{vector of y values}
  \item{groups}{vector of grouping variables (optional), in case of NULL all points are treated as one group}
  \item{plot.type}{plot type: possible arguments are 'density' to plot density lines, 'points' to plot points, or 'image' to plot point counts as a raster image. In case of NULL, the optimal plot.type will be determined dependent on the number of values. It is also possible to combine plot types (e.g. c("image", "density")).}
  \item{objfct}{Compute objective functions and add a 1:1 line to the plot?}
  \item{which.objfct}{Which objective functions should be added to legend of the plot if objfct=TRUE?}
  \item{add}{add plot to existing plot?}
  \item{density.levels}{number of levels for density lines	}
  \item{density.labels}{add labels to density lines?	}
  \item{image.nbrks}{minimum number of breaks for the image}
  \item{fits}{Fitting methods that should be used. See \code{\link{MultiFit}} for details. If several methods area provided the mean of all methods is plotted.}
  \item{fit.quantile}{Compute fit to a certain quantile? If NULL, fits to the mean. Otherwise sa value between 0 and 1 can be specified to fit to a certain quantile.}
  \item{fit.minnobs}{minimum number of observations per group in order to calculate a fitting curve}
  \item{fit.groups}{Plot fit lines for groups?}
  \item{fit.global}{Plot global fit lines?}
  \item{col.points}{color for the points/groups}
  \item{col.image}{color range for image}
  \item{col.density}{color range for density lines}
  \item{col.fit}{colors for fitting lines}
  \item{col.global}{colors for global fittiing line}
  \item{xlab}{a title for the x axis}
  \item{ylab}{a title for the y axis}
  \item{main}{an overall title for the plot}
  \item{xlim}{range for the x axis}
  \item{ylim}{range for the y axis}
  \item{lwd}{line width}
  \item{lty}{line type}
  \item{quantile.x}{lower and upper quantile to exclude extreme x values from plotting and fit calculations}
  \item{quantile.y}{lower and upper quantile to exclude extreme y values from plotting and fit calculations}
  \item{plot}{should a scatterplot be plotted or calculate only fitting lines}
  \item{cex}{size of the points in the plot. If NULL point size will be calculated based on the number of points.}
  \item{pch}{point symbol}
  \item{alpha}{transparency of the points (0...255)}
  \item{\dots}{further arguments to \code{\link{MultiFit}} or \code{\link{plot.default}}}
}
\details{This function plots a scatterplot.}
\value{The function returns a list with the computed fitting lines and objective functions per group.}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{

# create some data:
n <- 10000
x <- runif(n, 0, 100)
groups <- c(rep(1, 3000), rep(2, 3000), rep(3, 4000))
y <- (3 * x^(1/2) + rnorm(n, 0, x/20)) * groups

# standard plot: not very well distinguishable
plot(x, y)

# ScatterPlot 
result <- ScatterPlot(x, y)

# ScatterPlot with coulored groups and fitting lines
result <- ScatterPlot(x, y, groups)

# different plot types 
result <- ScatterPlot(x, y, plot.type="points")
result <- ScatterPlot(x, y, plot.type="density")
result <- ScatterPlot(x, y, plot.type=c("image", "density"))

# plot and compute objective functions
result <- ScatterPlot(x, y, groups, objfct=TRUE)
result

# plot fits to upper 0.9 and lower 0.1 quantiles, mean fit from two methods
result <- ScatterPlot(x, y, groups, fits=c("poly3", "spline"), 
 fit.quantile=0.9, plot.type="image", fit.global=FALSE)
result <- ScatterPlot(x, y, groups, fits=c("poly3", "spline"), 
 fit.quantile=0.1, plot.type=NA, add=TRUE, fit.global=FALSE)


}
