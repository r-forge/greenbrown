\name{TaylorDiagram}
\alias{TaylorDiagram}
\title{Taylor diagram}
\description{Plot a Taylor diagram. This is a modification of the function \code{\link{taylor.diagram}} in the plotrix package.}
\usage{TaylorDiagram(ref, model, groups = rep(1, length(model)), col = NULL, 
    plot.combined = FALSE, normalize = TRUE, sd.arcs = TRUE, 
    text.groups = NULL, text.ref = "Obs", text.combined = "All", 
    pos.cor = TRUE, ...)}
\arguments{
  \item{ref}{numeric vector - reference values}
  \item{model}{numeric vector - predicted model values}
  \item{groups}{numeric vector that split ref and model in different groups, e.g. to indicate different models or subsets of the data}
  \item{col}{color for each group, should have the same length as grouping elements}
  \item{plot.combined}{plot also the combined set, i.e. without splitting by groups?}
  \item{normalize}{whether to normalize the models so that the reference has a standard deviation of 1}
  \item{sd.arcs}{whether to display arcs along the standard deviation axes }
  \item{text.groups}{text labels for the groups}
  \item{text.ref}{text label for the reference}
  \item{text.combined}{text label for the combined model/data set without grouping}
  \item{pos.cor}{whether to display only positive (TRUE) or all values of correlation (FALSE). If NULL, this will depend on the correlations.}
  \item{\dots}{further arguments as in \code{\link{taylor.diagram}} }
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{BAmodel2IntegrationData}}}
\examples{

ref <- rnorm(50, 0, 5)
model1 <- ref + c(rnorm(25, 1, 2), rnorm(25, 4, 0.2))
model2 <- ref + c(rnorm(25, -5, 5), rnorm(25, 4, 0.2))
model3 <- ref + c(rnorm(25, 10, 10), rnorm(25, 4, 0.2))
data <- data.frame(ref=rep(ref, 3), model=c(model1, model2, model3), 
   group.models=rep(c("model1", "model2", "model3"), each=50), 
   group.models.subsets=rep(c("model1.subset1", "model1.subset2", "model2.subset1", "model2.subset2", "model3.subset1", "model3.subset2"), each=25))

TaylorDiagram(data$ref, data$model, data$group.models)
TaylorDiagram(data$ref, data$model, data$group.models.subsets)


}
