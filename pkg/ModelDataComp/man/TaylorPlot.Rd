\name{TaylorPlot}
\alias{TaylorPlot}
\title{Plot a Taylor diagram}
\description{Plot a Taylor diagram. This is a modification of the function \code{\link{taylor.diagram}} in the plotrix package.}
\usage{TaylorPlot(sim, obs, groups = rep(1, length(sim)), col = NULL, 
    plot.combined = FALSE, normalize = TRUE, sd.arcs = TRUE, 
    text.groups = NULL, text.obs = "Obs", text.combined = "All", 
    pos.cor = TRUE, ...)}
\arguments{
  \item{sim}{simulated/predicted/modeled values}
  \item{obs}{observed/reference values}
  \item{groups}{numeric vector that split ref and model in different groups, e.g. to indicate different models or subsets of the data}
  \item{col}{color for each group, should have the same length as grouping elements}
  \item{plot.combined}{plot also the combined set, i.e. without splitting by groups?}
  \item{normalize}{whether to normalize the models so that the reference has a standard deviation of 1}
  \item{sd.arcs}{whether to display arcs along the standard deviation axes }
  \item{text.groups}{text labels for the groups}
  \item{text.obs}{text label for the observations}
  \item{text.combined}{text label for the combined model/data set without grouping}
  \item{pos.cor}{whether to display only positive (TRUE) or all values of correlation (FALSE). If NULL, this will depend on the correlations.}
  \item{\dots}{further arguments as in \code{\link{taylor.diagram}} }
}
\details{No details.}

\references{Taylor, K.E., 2001. Summarizing multiple aspects of model performance in a single diagram. Journal of Geophysical Research 106, 7183-7192.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}, \code{\link{plot.ObjFct}}, \code{\link{WollMilchSauPlot}}}
\examples{

obs <- rnorm(50, 0, 5)
model1 <- obs + c(rnorm(25, 1, 2), rnorm(25, 4, 0.2))
model2 <- obs + c(rnorm(25, -5, 5), rnorm(25, 4, 0.2))
model3 <- obs + c(rnorm(25, 10, 10), rnorm(25, 4, 0.2))
data <- data.frame(obs=rep(obs, 3), model=c(model1, model2, model3), 
   group.models=rep(c("model1", "model2", "model3"), each=50), 
   group.models.subsets=rep(c("model1.subset1", "model1.subset2", 
   "model2.subset1", "model2.subset2", "model3.subset1", "model3.subset2"), each=25))

TaylorPlot(data$model, data$obs, data$group.models)
TaylorPlot(data$model, data$obs, data$group.models.subsets)


}
