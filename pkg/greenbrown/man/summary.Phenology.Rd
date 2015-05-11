\name{summary.Phenology}
\alias{summary.Phenology}
\title{Print a summary of phenology metrics}
\description{The function prints a summary the results from \code{\link{Phenology}}.}
\usage{\method{summary}{Phenology}(x)}\arguments{
  \item{x}{Object of class 'Phenology' as returned from function \code{\link{Phenology}}}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de> [aut, cre]}




\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate phenology metrics
phen <- Phenology(ndvi)
phen
print(phen)

# plot phenology metrics
plot(phen)
	
}
