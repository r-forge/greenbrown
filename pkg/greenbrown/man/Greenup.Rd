\name{Greenup}
\alias{Greenup}
\title{Identify greenup and senescence periods in time series}
\description{The function identifies 'greenup' (i.e. periods with increase) and 'senescence' (i.e. periods with decrease) in time series. This function implements threshold methods for phenology. Please use the function \code{\link{Phenology}} to apply this method.}
\usage{Greenup(x, ...)}\arguments{
  \item{x}{vector of values}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a boolean vector: TRUE (greenup) and FALSE (senescence). }

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Phenology}}}

