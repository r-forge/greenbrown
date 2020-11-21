\name{PhenoDeriv}
\alias{PhenoDeriv}
\title{Method 'Deriv' to calculate phenology metrics}
\description{This function implements the derivative method for phenology. This is rather an internal function; please use the function \code{\link{Phenology}} to apply this method.}
\usage{PhenoDeriv(x, min.mean = 0.1, 
    calc.pheno = TRUE, 
    plot = FALSE, ...)}
\arguments{
  \item{x}{seasonal cycle of one year}
  \item{min.mean}{minimum mean annual value in order to calculate phenology metrics. Use this threshold to suppress the calculation of metrics in grid cells with low average values	}
  \item{calc.pheno}{calculate phenology metrics or return NA?}
  \item{plot}{\code{plot} results?}
  \item{\dots}{further arguments (currently not used)}
}

\value{The function returns a vector with SOS, EOS, LOS, POP, MGS, RSP, RAU, PEAK, MSP and MAU. }

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{Phenology}}}
\examples{


data(ndvi)
plot(ndvi)

# perform time series preprocessing from first year of data
x <- TsPP(ndvi, interpolate=TRUE)[1:365]
plot(x)

# calculate phenology metrics for first year
PhenoDeriv(x, plot=TRUE)


}
