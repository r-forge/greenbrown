\name{AllTsteps}
\alias{AllTsteps}
\title{Convert an irregular zoo time series to a zoo time series with all time steps}
\description{Irregular 'zoo' time series with missing time steps are converted to a 'zoo' time series including all time steps. Observations at time steps that were missing in the original time series are filled with NA.}
\usage{AllTsteps(x, by = "day", start.jan = FALSE, end.dec = FALSE, 
    exclude.feb29 = FALSE, ...)}
\arguments{
  \item{x}{a time series of class 'zoo'}
  \item{by}{time step of full time series, e.g. "day", "month"}
  \item{start.jan}{Should the full time series start in January? If FALSE, the full time series will start at the first observation in x.}
  \item{end.dec}{Should the full time series end in December? If FALSE, the full time series will end at the last observation in x.}
  \item{exclude.feb29}{Should 29th Februaries be excluded in case of a daily time series (i.e. if by = 'day')?}
  \item{\dots}{further arguments (currently not used)}
}



\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{
x <- zoo(rnorm(5), as.Date(c("2010-01-15", "2010-02-15", "2010-07-15", 
 "2010-08-15", "2010-09-15")))
x
AllTsteps(x, by="month")
}
