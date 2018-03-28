\name{SofiaLogistic}
\alias{SofiaLogistic}
\title{Logistic function }
\description{Compute values of a logistic function as used in Sofia models.}
\usage{SofiaLogistic(par, x, ...)}
\arguments{
  \item{par}{parameters of logistic function, a vector of length 3 (upper asymptote, slope, turning point) or length 4 (upper asymptote, slope, turning point, lower asymptote)}
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
plot(x, SofiaLogistic(par, x), type="l")

par <- c(1, 0.2, 0)
plot(x, SofiaLogistic(par, x), type="l")

par <- c(10, -1, 0)
plot(x, SofiaLogistic(par, x), type="l")

# define also the lower asymptote as forth parameter
par <- c(1, 0.5, 0, 0.1)
plot(x, SofiaLogistic(par, x), type="l")

par <- c(1, 0.5, 0, -1)
plot(x, SofiaLogistic(par, x), type="l")

}
