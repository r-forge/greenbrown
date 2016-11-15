\name{plot.SofiaOpt}
\alias{plot.SofiaOpt}
\title{plot a SofiaOpt object}
\description{The optimization within \code{\link{SofiaFit}} produces files that can be used to restart or monitor an optimization experiment. These files can be read with \code{\link{ReadSofiaFit}} and plotted with this function..}
\usage{\method{plot}{SofiaOpt}(x, plot.objfct = c("Cor", "MEF", "Pbias"), ...)}
\arguments{
  \item{x}{an object of class \code{\link{SofiaFit}} as returned by \code{\link{ReadSofiaFit}}}
  \item{plot.objfct}{which objective function should be plotted (maximum 3)?}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{SofiaOpt}}}

