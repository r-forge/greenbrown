\name{CorPlot}
\alias{CorPlot}
\title{Correlation Matrix Plot}
\description{Plots a matrix of variables with #1 scatterplots in the lower triangle, #2 correlation values (or other \code{\link{ObjFct}} metrics) in the upper triangle, and #3 histograms along the diagonal.}
\usage{CorPlot(x, objfct = "Spearman", cols = NULL, ...)}
\arguments{
  \item{x}{data.frame or matrix with values that should be plotted}
  \item{objfct}{Which objective function metric should be shown in the upper triangle of the matrix?}
  \item{cols}{vector of colours that should be used}
  \item{\dots}{further arguments to plot functions}
}
\details{This function plots a matrix with scatter plots.}


\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{

# create some data
a <- 1:30
b <- 30:1
c <- rnorm(30)
d <- a + rnorm(30, 0, 5)
e <- b + rnorm(30, 0, 15)
f <- a * exp(c)
x <- cbind(a, b, d, e, f)

# default plot with Spearman correlation 
CorPlot(x)

# other objective function metrics
CorPlot(x, objfct="Cor")
CorPlot(x, objfct="KS")
CorPlot(x, objfct="IoA")
CorPlot(x, objfct="MEF")
CorPlot(x, objfct="RMSE")

# other colours
CorPlot(x, objfct="RMSE", cols=c("blue", "red"))

# with text
CorPlot(x, objfct="Cor", cols=c("blue", "red"), prefix="r = ")

}
