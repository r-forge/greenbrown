\name{InterpolateMatrix}
\alias{InterpolateMatrix}
\title{Interpolate NA values in a matrix using a moving mean}
\description{This function interpolates missing values in a matrix with the mean of the neighbouring matrix cells.}
\usage{InterpolateMatrix(m)}
\arguments{
  \item{m}{a matrix with NA value to interpolate}
}

\value{matrix with interpolated values}

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}




\examples{
m <- matrix(1:25, 5, 10)
m[sample(1:50, 10)] <- NA
m
InterpolateMatrix(m)
}
