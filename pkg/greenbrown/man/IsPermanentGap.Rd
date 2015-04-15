\name{IsPermanentGap}
\alias{IsPermanentGap}
\title{Identify if a gap in a time series occurs permanently}
\description{The function identifies obervations in time series as permanent gaps if the gap occurs during the same period in several years.}
\usage{IsPermanentGap(Yt, min.gapfrac = 0.2, lower = TRUE, ...)}\arguments{
  \item{Yt}{univariate time series of class \code{\link{ts}}}
  \item{min.gapfrac}{How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.}
  \item{lower}{identify lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)}
  \item{\dots}{further arguments (currently not used)}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{TsPP}}}
\examples{
# load NDVI data
data(ndvi)

# introduce some systematic gaps in january, february, december and july
gaps <- ndvi
gaps[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12 | cycle(ndvi) == 7] <- NA
gaps[1] <- 0.2
gaps[7] <- 0.3
plot(gaps)

# identifiy permanent winter gaps only
IsPermanentGap(gaps, lower=TRUE)

# identify permanent summer gaps
IsPermanentGap(gaps, lower=FALSE)

# identify all permanent gaps
IsPermanentGap(gaps, lower=NULL)
}
