\name{Logistic}
\alias{Logistic}
\title{Logistic function}
\description{Compute values of a logistic function.}
\usage{Logistic(par, x, ...)}
\arguments{
  \item{par}{parameters of logistic function, a vector of length 3 (asymptote, slope, turning point)}
  \item{x}{independent variable}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}
\value{a vector}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}}
\examples{
x <- -20:20
par <- c(1, 0.5, 0)
plot(x, Logistic(par, x), type="l")

par <- c(1, 0.2, 0)
plot(x, Logistic(par, x), type="l")

par <- c(10, -1, 0)
plot(x, Logistic(par, x), type="l")

par <- c(-2, -1, 0)
plot(x, Logistic(par, x), type="l")

}
