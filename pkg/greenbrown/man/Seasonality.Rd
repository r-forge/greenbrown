\name{Seasonality}
\alias{Seasonality}
\title{Check a time series for seasonality}
\description{This function checks a time series for seasonality using three different approaches: 
\itemize{ 
\item{ \code{'pgram'} computes a periodogram using fast fourier transformation (\code{\link{spec.pgram}}) and checks at which frequency the periodogram has a maximum. A maximum at a frequency of 1 indicates seasonality and the function returns TRUE. }
\item{ \code{'acf'} computes the auto-correlation function of the de-trended time series using \code{\link{acf}}. A minimum acf value at a lag of 0.5 indicates seasonality and the function returns TRUE. }
\item{ \code{'lm'} fits two linear models to the time series. The first model includes the trend and the seasonal cycle as factorial variable. The second model includes only the trend. Based on the \code{\link{BIC}} the better model is selected and the function returns TRUE if the first model (including a seasonal term) is better. }
}}
\usage{Seasonality(Yt, return.freq = FALSE, 
    plot = FALSE, ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{return.freq}{if \code{return.freq} is TRUE the function returns the frequency at the maximum of the periodogram.}
  \item{plot}{\code{plot} periodogram and acf? (see \code{\link{spec.pgram}} and \code{\link{acf}})}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a boolean vector of length 3 including TRUE if a method detected seasonality or FALSE if a method did not detect seasonality. }

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{spec.pgram}}, \code{\link{acf}}, \code{\link{lm}}, \code{\link{BIC}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)

# time series with strong Seasonality:
Yt <- SimTs(Srange = 0.2, Tslope=c(0.0004, 0))[,1]
plot(Yt)
Seasonality(Yt)

# time series with Seasonality and some noise
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.18, Rrange=0.25)[,1]
plot(Yt)
Seasonality(Yt)

# time series with Seasonality but many noise
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.22, Rrange=0.4)[,1]
plot(Yt)
Seasonality(Yt)

# time series without Seasonality 
Yt <- SimTs(Srange = 0.01, Tslope=c(0.0004, 0), Rsd=0.2, Rrange=0.4)[,1]
plot(Yt)
Seasonality(Yt)

# plot results for each seasonality method
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.18, Rrange=0.25)[,1]
Seasonality(Yt, plot=TRUE)


}
