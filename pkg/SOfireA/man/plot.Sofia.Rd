\name{plot.Sofia}
\alias{plot.Sofia}
\title{plot a Sofia object}
\description{Plots a \code{\link{Sofia}} object.}
\usage{\method{plot}{Sofia}(x, ylab = "y", mfrow = NULL, names = NULL, main = NULL, 
    plot.order = NULL, labels = paste0("(", letters, ")"), ...)}
\arguments{
  \item{x}{a 'Sofia' object}
  \item{ylab}{label for response variable}
  \item{mfrow}{number of rows and columns for the plot}
  \item{names}{names of the variables in the response functions}
  \item{main}{title of the plot}
  \item{plot.order}{Order for plotting of factors}
  \item{labels}{Labels for subplots. Set to NULL to avoid labels.}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}}
\examples{

# explanatory variables
sm <- 1:100
temp <- rnorm(100, 12, 10)
x <- cbind(sm, temp)

# fractional coverage of groups, e.g. plant functional types
tree <- runif(100, 0, 0.8)
grass <- 1 - tree
area <- cbind(tree, grass)

# with some more realisitc parameters:
par <- SofiaPar(colnames(x), per.group=c(TRUE, FALSE), group.names=c("tree", "grass"))
par$par <- c(1, 1, 20, 2, 1, -0.2, -0.1, 13, 10)
sf <- Sofia(x, area, per.group=c(TRUE, FALSE), sofiapar=par)
plot(sf)

}
