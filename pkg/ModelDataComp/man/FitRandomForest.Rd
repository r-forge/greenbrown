\name{FitRandomForest}
\alias{FitRandomForest}
\title{Fit a random forest to a data set}
\description{-}
\usage{FitRandomForest(x, y, nodes = 4, ntree = 500, mtry = NULL, xvars2 = NULL, 
    ...)}
\arguments{
  \item{x}{predictor variables}
  \item{y}{dependent variable}
  \item{nodes}{number of nodes for parallel compuation of random forest}
  \item{ntree}{number of trees}
  \item{mtry}{Number of variables randomly sampled as candidates at each split. If NULL code{\link{tuneRF}} will be used to determine an optimal value for mtry}
  \item{xvars2}{
}
  \item{\dots}{further arguments}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{FitDataModel}}}

