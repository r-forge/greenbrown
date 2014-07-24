\name{PlotPhenCycle}
\alias{PlotPhenCycle}
\title{plot a single seasonal cycle with phenology metrics}
\description{This function plots a seasonal cycle with phenology metrics}
\usage{PlotPhenCycle(x, xpred = NULL, metrics, xlab = "DOY", ylab = "NDVI", 
    trs = NULL, main = "", ...)}
\arguments{
  \item{x}{values of one year}
  \item{xpred}{smoothed/predicted values }
  \item{metrics}{vector of pheology metrics}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{trs}{threshold for threshold methods}
  \item{main}{title}
  \item{\dots}{further arguments (currently not used)}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Phenology}}}

