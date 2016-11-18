\name{TrendSSA}
\alias{TrendSSA}
\title{Trend estimation based on SSA (singluar spectrum analysis)}
\description{The function computes a non-linear trend based on \code{\link{ssa}}. Please note: Use the function \code{\link{TrendSeasonalAdjusted}} with the option funSeasonalCycle=SSASeasonalCylce to compute a linear trend with breakpoint detection based on a seasonal adjusted time series (method "SSA" as desribed in Forkel et al. 2013).}
\usage{TrendSSA(Yt, ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{\dots}{additional arguments (currently not used)}
}

\value{The function returns a list of class "Trend". }

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ssa}}}
\examples{
## load a time series of NDVI (normalized difference vegetation index)
#data(ndvi)
#plot(ndvi)
#	
## calculate trend on mean annual NDVI values
#trd <- TrendSSA(ndvi)
#trd
#plot(trd)

}
