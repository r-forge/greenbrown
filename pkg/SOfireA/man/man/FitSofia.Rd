\name{FitSofia}
\alias{FitSofia}
\title{Fit a Sofia model to a data set}
\description{The function fits a SOFIA model to a dataset. }
\usage{FitSofia(x, y, unc = NULL, per.group = rep(FALSE, ncol(x)), nodes = 4, 
    sofiapar, restart = 0, cost = NULL, ...)}
\arguments{
  \item{x}{data.frame with independent variables}
  \item{y}{dependent variable (observation)}
  \item{unc}{uncertainty of dependent variable}
  \item{per.group}{a boolean vector that indicates if a column in x acts per group (e.g. PFTs) }
  \item{nodes}{number of nodes for parallel compuation during genetic optimization}
  \item{sofiapar}{SofiaPar object with prior parameters}
  \item{restart}{restart previous Sofia optimization? 0 = start new, 1 = continue with previous, 2 = do post-processing}
  \item{cost}{cost function to be used}
  \item{\dots}{further arguments}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}, \code{\link{SofiaOpt}}}

