\name{print.Trend}
\alias{print.Trend}
\title{Print a summary of calculated trends}
\description{The function prints a summary the results from \code{\link{Trend}}.}
\usage{\method{print}{Trend}(x)}\arguments{
  \item{x}{Object of class 'Trend' as returned from function \code{\link{Trend}}}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}




\examples{
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# calculate a trend and look at the results
ndvi.trend <- Trend(ndvi)
ndvi.trend
print(ndvi.trend)
summary(ndvi.trend)
	
}
