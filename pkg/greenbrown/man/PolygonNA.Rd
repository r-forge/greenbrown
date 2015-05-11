\name{PolygonNA}
\alias{PolygonNA}
\title{Plot a polygon by accounting for NA values (breaks in polygon)}
\description{This function is an improved version of \code{\link{polygon}} that considers NA values in plotting.}
\usage{PolygonNA(x, lower, upper, col = "grey")}\arguments{
  \item{x}{vector of x-values}
  \item{lower}{vector of lower polygon range}
  \item{upper}{vector of upper polygon range}
  \item{col}{color of the polygon}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de> [aut, cre]}




\examples{

x <- 1:10
med <- rnorm(length(x))
lower <- med - 2
upper <- med + 2

# example 1: no NA values
plot(x, med, type="l", ylim=range(c(lower, upper), na.rm=TRUE))
PolygonNA(x, lower, upper)
lines(x, med)

# example 2: with some NA values 
lower1 <- lower
upper1 <- upper
lower1[c(1, 6, 10)] <- NA
upper1[c(1:2, 6)] <- NA
plot(x, med, type="l", ylim=range(c(lower, upper), na.rm=TRUE))
PolygonNA(x, lower1, upper1)
lines(x, med)


}
