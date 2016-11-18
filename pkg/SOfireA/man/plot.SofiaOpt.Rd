\name{plot.SofiaOpt}
\alias{plot.SofiaOpt}
\title{Plot SOFIA optimization results}
\description{Plots the devlopment of the cost and of other performance metrics during the optimization of a SOFIA model. \code{\link{SofiaOpt}} produces files that can be used to restart or monitor an optimization experiment. These files can be read with \code{\link{ReadSofiaOpt}} and plotted with this function..}
\usage{\method{plot}{SofiaOpt}(x, plot.objfct = c("IoA", "FV", "MEF"), ...)}
\arguments{
  \item{x}{an object of class \code{\link{SofiaOpt}} as returned by \code{\link{ReadSofiaOpt}}}
  \item{plot.objfct}{which objective function should be plotted (maximum 3)?}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SofiaOpt}}}

