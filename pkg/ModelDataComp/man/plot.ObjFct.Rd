\name{plot.ObjFct}
\alias{plot.ObjFct}
\title{Plot objective functions }
\description{Barplot or scatterplot of objective function metrics. A barplot is created if only one metric is defined in \code{which}. A scatterplot is created if two metrics are defined.}
\usage{\method{plot}{ObjFct}(x, which = c("Cor", "KS", "AE"), alpha = 0.1, cols = "black", 
    pch = NULL, legend = TRUE, xlab = NULL, ylab = NULL, xlim = NULL, 
    ylim = NULL, txt = NULL, add = FALSE, ...)}
\arguments{
  \item{x}{an object of class \code{\link{ObjFct}}}
  \item{which}{Which objective function metrics should be plotted? If one metric is defined, a barplot will be plotted. If more than one metric is defined, the first metric will be used for the x-axis and the second metric for y-axis of a scatterplot. If a third metric is specified, point sizes in the scatterplot will be scaled.}
  \item{alpha}{significance level to plot objetive functions that are not significant different from the optimum with different point symbols. This only works if the the selected objective functions in which have a p-value. Set to NULL to avoid this option. }
  \item{cols}{colors for the groups}
  \item{pch}{point symbols for scatterplots or for pointsplots (instead of barplot)}
  \item{legend}{plot a legend for the significance point symbols?}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{xlim}{limits for x-axis}
  \item{ylim}{limits for y-axis}
  \item{txt}{text for the points}
  \item{add}{add plot to an existing plot? This only works for scatterplots.}
  \item{\dots}{further arguments to \code{\link{plot}}}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}, \code{\link{TaylorPlot}}, \code{\link{ScatterPlot}}, \code{\link{WollMilchSauPlot}}}
\examples{

# create some data
obs <- 1:300
sim <- obs * c(rnorm(100, 1, 0.1), rnorm(100, 1, 0.3), rnorm(100, 1, 0.5))
groups <- c(rep("subset 1", 100), rep("subset 2", 100), rep("subset 3", 100))
ScatterPlot(sim, obs, groups, objfct=TRUE)

of <- ObjFct(sim, obs, groups)
of 

# default plot
plot(of)

# barplots with only one metric
plot(of, "Cor")
plot(of, "AE")
plot(of, "MEF")

# options for the scatterplot:
plot(of, c("Cor", "RMSE", "MEF"))
plot(of, c("Spearman", "KS"), alpha=0.05)
plot(of, c("Spearman", "KS"), alpha=NULL)
plot(of, cols=c("black", "red", "blue", "green"))
plot(of, c("Cor", "FV"), pch=0:4)

}
