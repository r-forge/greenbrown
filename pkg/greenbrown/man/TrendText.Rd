\name{TrendText}
\alias{TrendText}
\title{Text of trends in time series}
\description{The function computes a \code{trend} in a time series and creates text strings of the estimated \code{trend} that ca be used in plots.}
\usage{TrendText(Yt, trend = TrendAAT, 
    period = NULL, unit = "unit", 
    per.year = FALSE)}
\arguments{
  \item{Yt}{a time series}
  \item{trend}{a function how the \code{trend} should be computed, e.g. \code{\link{TrendAAT}}, \code{\link{TrendRQ}}, \code{\link{TrendSTM}}}
  \item{period}{an additional sub-period of x for which trends should be additionally computed}
  \item{unit}{\code{unit} of the values in \code{Yt}}
  \item{per.year}{Is \code{unit} per year? If TRUE, \code{trend} is in \% yr-2 !}
}
\details{This function is used in \code{\link{MtsPlot}}}
\value{A list with the estimated trends and with text.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{MtsPlot}}}
\examples{
trd <- TrendText(ndvi, period=c(1982, 1996), unit="")
plot(ndvi)
lines(trd$trd1, col="red", lwd=2)
lines(trd$trd2, col="blue", lwd=2, lty=2)
text(2003, 0.19, trd$full.perc, col="red", cex=1.5) # full trend in \%
text(2003, 0.36, trd$full.slope, col="red", cex=1.5) # full trend slope with unit
text(2003, 0.34, trd$full.slope2, col="red", cex=1.5) # full trend slope without unit
text(1988, 0.19, trd$subs.perc, col="blue", cex=1.5) # period: trend in \%
text(1988, 0.36, trd$subs.slope, col="blue", cex=1.5) # period: trend slope with unit
text(1988, 0.34, trd$subs.slope2, col="blue", cex=1.5) # period: trend slope without unit

}
