\name{TrendRunmed}
\alias{TrendRunmed}
\title{Trend estimation based on running median}
\description{The function computes a non-linear trend based a running median.}
\usage{TrendRunmed(Yt, k = NULL, ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{k}{integer width of median window; must be odd. If NULL a window size of 20 years (i.e. frequency * 20) will be used.}
  \item{\dots}{additional arguments (currently not used)}
}

\value{The function returns a list of class "Trend" with the following components:
\itemize{ 
\item{ \code{series} time series on which the trend was calculated. }
\item{ \code{trend} time series with the estimated trend component. }
\item{ \code{time} a vector of time steps. }
\item{ \code{pval} Mann-Kendall test p-value of the trend component. }
}}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{stl}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate trend on mean annual NDVI values
trd <- TrendRunmed(ndvi)
trd
plot(trd, symbolic=TRUE)

}
