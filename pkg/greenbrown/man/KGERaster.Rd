\name{KGERaster}
\alias{KGERaster}
\title{Compute Kling-Gupta efficiency and related metrics of two multi-layer raster data sets}
\description{This function can be used to apply the function \code{\link{KGE}} on raster data. See \code{\link{KGE}} for details.}
\usage{KGERaster(x, ref, trend = FALSE, start = c(1982, 1), freq = 12, 
    ...)}
\arguments{
  \item{x}{multi-layer raster object of class \code{\link[raster]{brick}} including modelled time series}
  \item{ref}{multi-layer raster object of class \code{\link[raster]{brick}} including reference (observed or standard model run) time series}
  \item{trend}{Include the effect of trend in the calculation? (default: FALSE). The calculation of breakpoints is currently not implemented for the function KGERaster.}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.}
  \item{\dots}{further arguments for the function \code{\link{calc}}}
}
\details{See \code{\link{KGE}} for details.}
\value{The function returns a raster brick with the following layers:
\itemize{ 
\item{ \code{KGE} Kling-Gupta effciency = 1 - eTotal }
\item{ \code{eTotal} total effect, i.e. euclidean distance }
\item{ \code{fMean} fraction of mean to the total effect}
\item{ \code{fVar} fraction of variance to the total effect }
\item{ \code{fCor} fraction of correlation to the total effect }
\item{ \code{fTrend} fraction of trend to the total effect (only if trend=TRUE)}
\item{ \code{eMean} effect of mean}
\item{ \code{eVar} effect of variance }
\item{ \code{eCor} effect of correlation  }
\item{ \code{eTrend} effect of trend (only if trend=TRUE)}	
}}
\references{Gupta, H. V., H. Kling, K. K. Yilmaz, G. F. Martinez (2009):  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology 377, 80-91.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{KGE}}, \code{\link{Trend}}}
\examples{
# # load a map of NDVI (normalized difference vegetation index) time series
# data(ndvimap)
# plot(ndvimap)

# # increase mean
# ndvimap2 <- ndvimap + 0.01
# kge1.r <- KGERaster(x=ndvimap2, ref=ndvimap)
# plot(kge1.r)

# # increase mean and variance
# ndvimap3 <- ndvimap + 0.01 + rnorm(1000, 0, 0.05)
# kge2.r <- KGERaster(ndvimap3, ndvimap)
# plot(kge2.r)

# # check also effects on trend (takes more time because of trend calculations)
# kge3.r <- KGERaster(ndvimap3, ndvimap, trend=TRUE)
# plot(kge3.r)
	
}
