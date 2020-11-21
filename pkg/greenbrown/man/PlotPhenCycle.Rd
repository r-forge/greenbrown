\name{PlotPhenCycle}
\alias{PlotPhenCycle}
\title{Plot a easonal cycle with phenology metrics}
\description{This function plots a seasonal cycle with phenology \code{metrics}.}
\usage{PlotPhenCycle(x, xpred = NULL, 
    metrics, xlab = "DOY", 
    ylab = "NDVI", trs = NULL, 
    main = "", ...)}
\arguments{
  \item{x}{values of one year}
  \item{xpred}{smoothed/predicted values }
  \item{metrics}{vector of pheology \code{metrics}}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{trs}{threshold for threshold methods}
  \item{main}{title}
  \item{\dots}{further arguments (currently not used)}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Phenology}}}
\examples{

data(ndvi)
plot(ndvi)

# perform time series preprocessing for first year of data
x <- TsPP(ndvi, interpolate=TRUE)[1:365]
plot(x)

# calculate phenology metrics for first year
metrics <- PhenoTrs(x, approach="White")
PlotPhenCycle(x, metrics=metrics)

}
