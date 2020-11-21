\name{print.Phenology}
\alias{print.Phenology}
\title{Prints phenology metrics}
\description{The function prints an object of class \code{\link{Phenology}}.}
\usage{\method{print}{Phenology}(x, ...)}
\arguments{
  \item{x}{Object of class 'Phenology' as returned from function \code{\link{Phenology}}}
  \item{\dots}{further arguments (not used)}
}



\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}




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
