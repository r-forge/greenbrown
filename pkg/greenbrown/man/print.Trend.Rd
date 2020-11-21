\name{print.Trend}
\alias{print.Trend}
\title{Prints trends}
\description{The function prints an object of class \code{\link{Trend}}.}
\usage{\method{print}{Trend}(x, ...)}
\arguments{
  \item{x}{Object of class 'Trend' as returned from function \code{\link{Trend}}}
  \item{\dots}{further arguments (not used)}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}




\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
	
}
