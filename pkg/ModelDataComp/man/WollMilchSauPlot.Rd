\name{WollMilchSauPlot}
\alias{WollMilchSauPlot}
\title{Compare means, distributions, and correlations of different datasets }
\description{Plots a \code{\link{pirateplot}} with couulors according to model performance. The function can be used to compare means, distributions, and correlations (or any other metric from \code{\link{ObjFct}}) between different datasets and models. Thus this plot is almost an "eier-legende Wollmichsau" (german, animal that produces eggs, wool, milk and meet) for model-data comparison. The plot is based on the function \code{\link{pirateplot}}}
\usage{WollMilchSauPlot(x, ref = 1, objfct = "Cor", cols = tim.colors(10), 
    brks = NULL, names = NULL, main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, ylim = NULL, legend = TRUE, legend.only = FALSE, 
    point.pch = NA, bar.o = 1, bean.o = 1, inf.o = 0, bean.lwd = 1, 
    line.lwd = 1, cut.min = NULL, cut.max = NULL, ...)}
\arguments{
  \item{x}{a data.frame with at least two columns}
  \item{ref}{Which column in x is the reference dataset? Also more than one reference can be provided, e.g. ref = c(1,2) will compute the objfct based on the combination of both datasets. }
  \item{objfct}{Which objective function metric should be used to create the colour palette? (see \code{\link{ObjFct}})}
  \item{cols}{vector of colors from which the color palette should be interpolated}
  \item{brks}{break for colour scale}
  \item{names}{names of the datasets}
  \item{main}{title of the plot}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{xlim}{limits for x-axis}
  \item{ylim}{limits for y-axis}
  \item{legend}{plot a legend?}
  \item{legend.only}{plot only a legend?}
  \item{point.pch}{point types, default: no points}
  \item{bar.o}{opacity of bars}
  \item{bean.o}{opacity of beans}
  \item{inf.o}{inference bands}
  \item{bean.lwd}{line width of beans}
  \item{line.lwd}{line width of lines}
  \item{cut.min}{Optional minimum value of the beans.}
  \item{cut.max}{Optional maximum value of the beans.}
  \item{\dots}{further arguments to \code{\link{pirateplot}}}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{pirateplot}}, \code{\link{ObjFct}}, \code{\link{TaylorPlot}}, \code{\link{WollMilchSauPlot}}, \code{\link{ScatterPlot}}}
\examples{

# create some data
obs <- rlnorm(500, 1, 1) # observations
sim1 <- obs * rnorm(500, 1, 0.5) # similar to obs
sim2 <- obs * rnorm(500, 1, 2) # less similar to obs
sim3 <- obs * rnorm(500, 1, 4) # less similar to obs
sim4 <- rlnorm(500, 1, 1) # same distribution but no correlation
sim5 <- rnorm(500, 4.4, 2) # similar mean but different distribution
x <- data.frame(obs, sim1, sim2, sim3, sim4, sim5)
x[x < 0] <- 0

# default plot
WollMilchSauPlot(x)

# with different objective function as colour
WollMilchSauPlot(x, objfct="IoA")
WollMilchSauPlot(x, objfct="Pbias")
WollMilchSauPlot(x, objfct="FV")

# some other settings, e.g. with plotting points
WollMilchSauPlot(x, ylab="Area (km2)", xlab="Groups", main="Comparison", 
 point.pch=16, bar.o=0.5, point.o=0.4)

# different color palettes
WollMilchSauPlot(x, cols=c("blue", "red"))
WollMilchSauPlot(x, cols=rainbow(10))
WollMilchSauPlot(x, cols=heat.colors(5))

# without legend (but using an objective function to colour)
WollMilchSauPlot(x, legend=FALSE)

# only legend
WollMilchSauPlot(x, legend.only=TRUE)

# without using an objective function 
WollMilchSauPlot(x, objfct=NULL)

# different example data
obs <- rnorm(500, 5, 1)
sim1 <- obs * rnorm(500, 1, 0.2) # similar to obs
sim2 <- obs * rnorm(500, 2, 1) # bias
sim3 <- obs * rlnorm(500, 1, 0.1) # less similar to obs but highly correlated
x <- data.frame(obs, sim1, sim2, sim3)
WollMilchSauPlot(x)


}
