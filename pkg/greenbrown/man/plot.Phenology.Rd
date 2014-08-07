\name{plot.Phenology}
\alias{plot.Phenology}
\title{Create time series plots with phenology metrics}
\description{This is the standard plot function for results of the \code{\link{Phenology}} function. See \code{\link{plot.default}} for further specifications of basic plots.}
\usage{\method{plot}{Phenology}(x, type = c("sos", "eos", "pop"), ylab = NULL, 
    ylim = NULL, add = FALSE, col = "black", add.trend = TRUE, 
    ...)}
\arguments{
  \item{x}{Object of class 'Phenology' as returned from function \code{\link{Phenology}}}
  \item{type}{varaible names that should be plotted from the \code{\link{Phenology}} object}
  \item{ylab}{a title for the y axis}
  \item{ylim}{limits for y-axis}
  \item{add}{add time series to exisiting plot?}
  \item{col}{line colors}
  \item{add.trend}{add trend lines to phenology time series?}
  \item{\dots}{Further arguments that can be passed \code{\link{plot.default}}}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.default}}, \code{\link{plot.ts}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate phenology metrics
phen <- Phenology(ndvi)
phen

# plot phenology metrics
plot(phen)
	
}