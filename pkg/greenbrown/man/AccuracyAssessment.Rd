\name{AccuracyAssessment}
\alias{AccuracyAssessment}
\title{Accuracy assessment from a contingency table}
\description{This function takes a contingency table as calculated with \code{\link{table}} or \code{\link[raster]{crosstab}} and computes an accuracy assessment, including the total accuracy, the user accuracy and the producer accuracy.}
\usage{AccuracyAssessment(tab)}\arguments{
  \item{tab}{contingency table as calculated with \code{\link{table}} or \code{\link{crosstab}}}
}

\value{The function returns the same frequency table as the input but with added row and column totals and total accuracy, user accuracy and producer accuracy.}
\references{Congalton, R.G. (1991): A review of assessing the accuracy of classifications of remotely sensed data. - Remote Sensing of Environment 1991, 37, 35–46.}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{CompareClassification}}, \code{\link{Kappa}}, \code{\link{TrendClassification}}	}
\examples{

# two classifications: 
a <- c(1, 1, 1, 2, 2, 2, 3, 4, 5, 5, 5, 1, 1, 1, 2, 2, 2, 3, 4, 5, 5, 3, 3, 2, 2)
b <- c(1, 2, 1, 2, 2, 2, 3, 4, 2, 2, 5, 1, 2, 2, 2, 1, 2, 3, 4, 5, 5, 3, 3, 2, 2)

# calculate first a contingency table
tab <- table(a, b)

# calculate now the accuracy assessment
AccuracyAssessment(tab)

# calculate Kappa coeffcient
Kappa(tab)

}
