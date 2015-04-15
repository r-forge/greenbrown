\name{TrendUncertainty}
\alias{TrendUncertainty}
\title{Compute uncertainties in trend statistics according to different start and end dates}
\description{The function computes trend statistics (linear trend slope and intercept, Mann-Kendall tau and p-value) with associated uncertainties (standard deviation) by sampling the time series according to different start and end dates using the function \code{\link{TrendSample}}}
\usage{TrendUncertainty(Yt, bp = NoBP(), sample.method = c("sample", 
    "all", "none"), sample.min.length = 0.75, sample.size = 30, 
    fun.unc = NULL)}\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{bp}{detected breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}}
  \item{sample.method}{Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). }
  \item{sample.min.length}{Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.}
  \item{sample.size}{sample size (number of combinations of start and end dates) to be used if \code{method} is sample.}
  \item{fun.unc}{function to summarize the uncertainty of the trend (default: quantile 0.05 and 0.95). Can be also 'sd' or other functions.}
}

\value{The function returns a data.frame with the estimated Mann-Kendall tau, p-value and slope and intercept of a linear trend with uncertainties defined as the standard deviation of these estimates dependent on different start and end dates.}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Trend}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# aggregate time series to annual time steps
ndvi <- aggregate(ndvi, FUN=mean)
plot(ndvi)

# compute trend statistics dependent on start and end of the time series
trd.ens <- TrendSample(ndvi)
plot(trd.ens)

# compute statistics for trend
TrendUncertainty(ndvi)

# compute trend statistics with uncertainties by considering breakpoints
bp <- breakpoints(ndvi ~ time(ndvi))
trd.unc <- TrendUncertainty(ndvi, bp)
trd.unc
trd.unc[[1]]$slope_unc


}
