\name{SelectQuantiles}
\alias{SelectQuantiles}
\title{Select values around quantiles}
\description{The function selects  from a set of points along the range of x values these points that are close to a certain quantile of y. First, x values are classified in n groups. Secondly, the specified quantile of y is computed for each group. Thirdly, all y values in a group that are outside +- 0.05 of the estimated quantile are removed. See examples for an illustration.}
\usage{SelectQuantiles(x, y, q = 0.5, n = NULL, ...)}
\arguments{
  \item{x}{vector of x values}
  \item{y}{vector of y values}
  \item{q}{quantile value for which data should be selected}
  \item{n}{number of classes for x}
  \item{\dots}{further arguments}
}



\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{

# x and y points
x <- 1:1000
y <- x * rnorm(1000, 1, 3)
plot(x, y)

# select points that are close to the median
q05 <- SelectQuantiles(x, y)
points(q05$x, q05$y, col="red")

# select points that are close to the 0.9 quantile
q09 <- SelectQuantiles(x, y, 0.9)
points(q09$x, q09$y, col="blue")

# select points that are close to the 0.1 quantile
q01 <- SelectQuantiles(x, y, 0.1)
points(q01$x, q01$y, col="purple")

# the selected points can be used for fits to a specific quantile
abline(lm(y ~ x, q09), col="blue")
abline(lm(y ~ x, q01), col="purple")


}
