\name{MannKendallSeg}
\alias{MannKendallSeg}
\title{Apply MannKendall test for segments of a time series}
\description{The function call \code{\link{MannKendall}} for different segments of a time series}
\usage{MannKendallSeg(Yt, seg = NULL, 
    bp = NoBP())}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{seg}{a vector indicating segments of a time series. If NULL, provide \code{bp}}
  \item{bp}{detected breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}}
}

\value{The function returns a data.frame with the estimated Mann-Kendall tau and p-value for each segment of the time series.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{MannKendall}}}
\examples{

# aggregate time series to annual time steps
ndvi <- aggregate(ndvi, FUN=mean)
plot(ndvi)

# MannKendall test for different segments
seg <- c(rep(1, length(start(ndvi)[1]:1995)), rep(2, length(1996:end(ndvi)[1])))
mk <- MannKendallSeg(ndvi, seg)
mk

# MannKendall test for segments that are defined by breakpoints
bp <- breakpoints(ndvi ~ time(ndvi))
mk <- MannKendallSeg(ndvi, bp=bp)
mk

# MannKendall test without segments
MannKendallSeg(ndvi)

}
