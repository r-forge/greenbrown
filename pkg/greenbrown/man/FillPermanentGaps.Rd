\name{FillPermanentGaps}
\alias{FillPermanentGaps}
\title{Fill permanent gaps in time series}
\description{Satellite time series are often affected by permanent gaps like missing observations during winter periods. Often time series methods can not deal with missing observations and require gap-free data. This function fills winter gaps with a constant fill value or according to the approach described in Beck et al. (2006).}
\usage{FillPermanentGaps(Yt, 
    min.gapfrac = 0.2, 
    lower = TRUE, fillval = NA, 
    fun = min, ...)}
\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{min.gapfrac}{How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if \code{min.gapfrac} = 0.4.}
  \item{lower}{fill \code{lower} gaps (TRUE), upper gaps (FALSE) or \code{lower} and upper gaps (NULL)}
  \item{fillval}{constant fill values for gaps. If NA the fill value will be estimated from the data using \code{fun}. }
  \item{fun}{function to be used to compute fill values. By default, minimum.}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a time series with filled permanent gaps.}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{TsPP}}}
\examples{

# sample some winter months to be set as gaps
winter <- (1:length(ndvi))[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12]
gaps <- sample(winter, length(winter)*0.3)

# introduce systematic winter gaps in time series
ndvi2 <- ndvi
ndvi2[gaps] <- NA
plot(ndvi2)
IsPermanentGap(ndvi2)

# fill winter with observed minimum
fill <- FillPermanentGaps(ndvi2)	
plot(fill, col="red"); lines(ndvi)

# fill winter with mean
fill <- FillPermanentGaps(ndvi2, fun=mean)	
plot(fill, col="red"); lines(ndvi)

# fill winter with 0
fill <- FillPermanentGaps(ndvi2, fillval=0)	
plot(fill, col="red"); lines(ndvi)

}
