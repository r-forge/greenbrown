\name{AnomaliesFiltersLags}
\alias{AnomaliesFiltersLags}
\title{Calculate anomalies, lags and rolling windows}
\description{This function computes several time-variant statisttics of a time series like seasonal anomalies, time lagged versions of time series, and \code{filters} time series based on running windows (using \code{\link{rollapply}}.}
\usage{AnomaliesFiltersLags(x, 
    funSeasonalCycle = MeanSeasonalCycle, 
    funFilter = median, 
    alignFilter = c("center", 
        "left", "right"), 
    filters = c(3, 5, 
        7, 9, 11, 13), 
    lags = -1:-7, anom = TRUE, 
    ...)}
\arguments{
  \item{x}{univariate time series of class \code{\link{ts}}}
  \item{funSeasonalCycle}{a function to estimate the seasonal cycle of the time series.}
  \item{funFilter}{a function to filter the time series based on rolling windows.}
  \item{alignFilter}{specifies whether the index of the running filter results should be left- or right-aligned or centered (default) compared to the rolling window of observations. See \code{\link{rollapply}}}
  \item{filters}{window sizes for rolling \code{filters} to be applied. If NULL, do not apply \code{filters}.}
  \item{lags}{time \code{lags} to be applied for lagged time series. If NULL, do not apply \code{lags}.}
  \item{anom}{compute seasonal anomalies?}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a multivariate time series of class 'mts' with the following columns:
\itemize{ 
\item{ \code{orig} the original time series }
\item{ \code{msc} mean seasonal cycle as computed with \code{funSeasonalCycle} (repeated for the full time series length) }
\item{ \code{anom} anomalies releative to mean seasonal cycle }
\item{ \code{orig.filterX} rolling window result based on the original time series as computed with \code{funFilter} for the filter window size X }
\item{ \code{anom.filterX} rolling window result based on the anomaly time series as computed with \code{funFilter} for the filter window size X }
\item{ \code{orig.lagX} time lagged version of the original time series for the time lag X }	
\item{ \code{msc.lagX} time lagged version of the mean seasonal cycle time series for the time lag X }		 
\item{ \code{anom.lagX} time lagged version of the anomaly time series for the time lag X }     
}}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{MeanSeasonalCycle}}}
\examples{

# do calculations
afl <- AnomaliesFiltersLags(ndvi)
summary(afl)

# seasonal anomalies
plot(afl[,"anom"]) 

# running median filters on original time series
plot(afl[, grep("orig.filter", colnames(afl))], plot.type="single", col=rainbow(6)) 

# running median filters on anomalies
plot(afl[, grep("anom.filter", colnames(afl))], plot.type="single", col=rainbow(6)) 

# lagged versions of original time series
plot(window(afl[, grep("orig.lag", colnames(afl))], start=c(1995, 1), 
   end=c(2000, 12)), plot.type="single", col=rainbow(7), type="l") 

# lagged versions of anomaly time series
plot(afl[, grep("anom.lag", colnames(afl))], plot.type="single", col=rainbow(7)) 

}
