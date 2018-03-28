\name{SofiaPar}
\alias{SofiaPar}
\title{Parameters for SOFIA models}
\description{The function creates an object of class 'SofiaPar' (which is actually a list) which contains information about Sofia model parameters.}
\usage{SofiaPar(x.names, per.group = rep(FALSE, length(x.names)), group.names = NULL, 
    par.act = NULL, par.prior = NULL, par.lower = NULL, par.upper = NULL, 
    par.priorsd = NULL, par.optim = NULL, ...)}
\arguments{
  \item{x.names}{names of independent variables}
  \item{per.group}{a boolean vector that indicates if a column in x acts per group (e.g. PFTs) }
  \item{group.names}{names of groups}
  \item{par.act}{
}
  \item{par.prior}{prior parameters}
  \item{par.lower}{lower parameter limits}
  \item{par.upper}{upper parameter limits}
  \item{par.priorsd}{uncertainty of prior parameters}
  \item{par.optim}{(boolean) parameters that shoud be included in optimization}
  \item{\dots}{further arguments}
}
\details{No details.}
\value{An object of class 'SofiaPar', which is actually a list.}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}, \code{\link{SofiaLogistic}}}
\examples{

# explanatory variables
sm <- 1:100
temp <- rnorm(100, 12, 10)
x <- cbind(sm, temp)

# fractional coverage of groups, e.g. plant functional types
tree <- runif(100, 0, 0.8)
grass <- 1 - tree
area <- cbind(tree, grass)

# parameters for SOFIA models
par <- SofiaPar(colnames(x), per.group=c(TRUE, FALSE), group.names=c("tree", "grass"))
par

}
