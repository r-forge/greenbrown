\name{TsPP}
\alias{TsPP}
\title{Pre-processing of time series}
\description{This function can be used for pre-processing of time series before the analyzing phenology or trends. The pre-processing involves the following steps: 
\itemize{ 
\item{ Step 1. Filling of permanent gaps. Values that are missing in each year will be filled using the function \code{\link{FillPermanentGaps}}.  }
\item{ Step 2. Temporal smoothing, gap filling and interpolation. The time series will be smoothed and remaining gaps will be filled. Optionally, time series will be interpolated to daily values. }
}}
\usage{TsPP(Yt, fpg = FillPermanentGaps, 
    tsgf = TSGFspline, 
    interpolate = FALSE, 
    min.gapfrac = 0.2, 
    lower = TRUE, fillval = NA, 
    fun = min, backup = NULL, 
    check.seasonality = 1:3, 
    ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}.}
  \item{fpg}{Filling of permanent gaps: If NULL, permanent gaps will be not filled, else the function \code{\link{FillPermanentGaps}} will be applied.}
  \item{tsgf}{Temporal smoothing and gap filling: Function to be used for temporal smoothing, gap filling and interpolation of the time series. If NULL, this step will be not applied. Otherwise a function needs to be specified. Exisiting functions that can be applied are \code{\link{TSGFspline}}, \code{\link{TSGFdoublelog}}  }
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values?}
  \item{min.gapfrac}{How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if \code{min.gapfrac} = 0.4.	}
  \item{lower}{For filling of permanent gaps: fill \code{lower} gaps (TRUE), upper gaps (FALSE) or \code{lower} and upper gaps (NULL)}
  \item{fillval}{For filling of permanent gaps: constant fill values for gaps. If NA the fill value will be estimated from the data using \code{fun}. }
  \item{fun}{For filling of permanent gaps: function to be used to compute fill values. By default, minimum.}
  \item{backup}{Which \code{backup} algorithm should be used instead of \code{\link{TSGFdoublelog}} for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFlinear"	}
  \item{check.seasonality}{Which methods in \code{\link{Seasonality}} should indicate TRUE (i.e. time series has seasonality) in order to calculate phenology metrics? 1:3 = all methods should indicate seasonality, Set to NULL in order to not perform seasonality checks.}
  \item{\dots}{further arguments (currently not used)}
}

\value{pre-processed time series}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{FillPermanentGaps}}}
\examples{
# introduce systematic gaps in winter and random gaps
gaps <- ndvi
gaps[runif(50, 1, length(ndvi))] <- NA
gaps[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12] <- NA
plot(gaps)

# perform pre-processing of time series using different methods 
pp.lin <- TsPP(gaps, tsgf=TSGFlinear) # linear interpolation + running median
pp.spl <- TsPP(gaps, tsgf=TSGFspline) # smoothing splines

plot(gaps)
cols <- rainbow(5)
lines(pp.lin, col=cols[1])
lines(pp.spl, col=cols[2])

data.df <- ts.union(time(gaps), orig=ndvi, pp.lin, pp.spl)
plot(data.df)
cor(na.omit(data.df[is.na(gaps),]))

}
