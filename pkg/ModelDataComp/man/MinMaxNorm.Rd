\name{MinMaxNorm}
\alias{MinMaxNorm}
\title{Normalize values between to a range}
\description{Normalizes values to a given range.}
\usage{MinMaxNorm(x, ext = c(0, 1), ...)}
\arguments{
  \item{x}{values}
  \item{ext}{new minimum and maximum values}
  \item{\dots}{further arguments (unused)}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{

x <- rnorm(10, 0, 2)
n <- MinMaxNorm(x)
plot(x, n)

}
