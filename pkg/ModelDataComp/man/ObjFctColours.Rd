\name{ObjFctColours}
\alias{ObjFctColours}
\title{Class breaks and colours for objective functions}
\description{The function computes class breaks and colour palettes for \code{\link{ObjFct}} metrics }
\usage{ObjFctColours(x, which = NULL, cols = NULL, ...)}
\arguments{
  \item{x}{Either an object of class \code{\link{ObjFct}} or a vector with values of an objective function metrics}
  \item{which}{which objective function metric should be used to create colours and class breaks?}
  \item{cols}{vector of colours that should be used}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}
\value{A list with class breaks and colours}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{

# create some data and compute objective functions
obs <- 1:100 # 'observations'
sim <- obs * c(rnorm(25, 1, 0.2), rnorm(25, 1, 0.5), rnorm(25, 1.5, 1), rnorm(25, -0.3, 0.3))
groups <- c(rep("subset 1", 25), rep("subset 2", 25), rep("subset 3", 25), rep("subset 4", 25))
ScatterPlot(obs, sim, groups)
of <- ObjFct(sim, obs, groups=groups)
of

# create colours and class breaks: default for correlation
cols <- ObjFctColours(of)
barplot(of[["Cor"]], col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# create colours and class breaks for RMSE
cols <- ObjFctColours(of, "RMSE")
barplot(of[["RMSE"]], col=cols$cols)
legend("top", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# create colours and class breaks based on vectors
ioa <- of[["IoA"]]
cols <- ObjFctColours(ioa, "IoA")
barplot(ioa, col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# use different colours
cols <- ObjFctColours(ioa, "IoA", cols=c("red", "green", "blue"))
barplot(ioa, col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

}
