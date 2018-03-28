\name{predict.Sofia}
\alias{predict.Sofia}
\title{Predict values based on a 'Sofia' object}
\description{Make a predicition based on a \code{\link{Sofia}} object and newdata}
\usage{\method{predict}{Sofia}(object, newdata, return.all = FALSE, ...)}
\arguments{
  \item{object}{an object of class 'Sofia', see \code{\link{Sofia}}}
  \item{newdata}{a data frame with columns names as in object$group.names for area fractions of groups and as in object$x.names for explantory variables}
  \item{return.all}{return all Sofia results? If FALSE, returns only total burned area}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}
\value{A vector with predicted values.}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}, \code{\link{SofiaOpt}}}

