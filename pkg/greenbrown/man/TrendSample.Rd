\name{TrendSample}
\alias{TrendSample}
\title{Compute trend statistics by sampling a time series according to different start and end dates}
\description{The function computes an ensemble of \code{trend} statistics (linear \code{trend} slope, Mann-Kendall tau and p-value) on a time series by sampling different start and end dates of the time series. This ensemble can be used to compute uncertainties in \code{trend} statistics. Results can be plotted using the function \code{\link{plot.TrendSample}}.}
\usage{TrendSample(Yt, sample.method = c("all", 
    "sample", "none"), 
    sample.min.length = 0.7, 
    sample.size = 30, 
    trend = TrendAAT)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{sample.method}{Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), \code{trend} statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", \code{trend} statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", \code{trend} statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). }
  \item{sample.min.length}{Minimum length of the time series (as a fraction of total length) that should be used to compute \code{trend} statistics. Time windows between start and end that are shorter than min.length will be not used for \code{trend} computation.}
  \item{sample.size}{sample size (number of combinations of start and end dates) to be used if \code{method} is sample.}
  \item{trend}{method that should be used to compute the \code{trend}}
}

\value{The function returns a data.frame with the start date, end date and length of the sample from the time series and the correspondig Mann-Kendall tau, p-value, slope, intercept, and percentage slope of a linear \code{trend}.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Trend}}, \code{\link{plot.TrendSample}}}
\examples{
# calculate uncertainty of trend dependent on start and end dates
trd.ens <- TrendSample(ndvi, trend=TrendAAT)
plot(trd.ens)
plot(trd.ens, "tau")

trd.ens <- TrendSample(ndvi, trend=TrendRQ)
plot(trd.ens)

}
