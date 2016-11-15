\name{RandomLHSPar}
\alias{RandomLHSPar}
\title{Latin-hypercube sampling of parameters}
\description{RandomLHSPar samples parameters based on random Latin-Hypercube sampling within given parameter ranges}
\usage{RandomLHSPar(nexp, lower, upper, fixed = rep(FALSE, length(lower)))}
\arguments{
  \item{nexp}{number of samples}
  \item{lower}{numeric vector of lower boundary values for each parameter}
  \item{upper}{numeric vector of upper borundary values for each parameter}
  \item{fixed}{boolean vector: should a parameter kept fixed (TRUE) or should all parameters be sampled (FALSE). If TRUE than the parameter value from the vector lower will be returned.	}
}

\value{The function returns a data.frame with sampled parameters.}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{

lower <- c(0, 100, -50, 1000)
names(lower) <- letters[1:4]
upper <- c(10, 500, 50, 10000)
RandomLHSPar(10, lower, upper)

}
