\name{KGETrendUncertainty}
\alias{KGETrendUncertainty}
\title{Compute uncertainty of Kling-Gupta efficiency based on beginning and end of time series }
\description{This function samples time series for different combinations of start and end years and computes for each combination the KGE (see \code{\link{KGE}}).}
\usage{KGETrendUncertainty(x, ref, trend = TRUE, eTrend.ifsignif = FALSE, 
    sample.method = c("sample", "all", "none"), sample.min.length = 0.75, 
    sample.size = 30, ...)}\arguments{
  \item{x}{time series from model result or factorial model run}
  \item{ref}{reference time series (observation or standard model run)}
  \item{trend}{Include the effect of trend in the calculation? }
  \item{eTrend.ifsignif}{compute effect on trend only if trend in reference series is significant, if FALSE compute always effect on trend (if trend = TRUE)}
  \item{sample.method}{Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). }
  \item{sample.min.length}{Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.}
  \item{sample.size}{sample size (number of combinations of start and end dates) to be used if \code{method} is sample.}
  \item{\dots}{further arguments for the function \code{\link{Trend}}}
}
\details{...}
\value{The function returns a data.frame with the following components:
\itemize{ 
\item{ \code{start} start of the time series }
\item{ \code{end} end of the time series }
\item{ \code{length} length of the time series }
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
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Trend}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# change the variance and compute effect
x <- ndvi + rnorm(length(ndvi), 0, 0.01)
plot(x, ndvi); abline(0,1)
unc <- KGETrendUncertainty(x, ndvi)
hist(unc$KGE)
	
}
