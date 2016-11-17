\name{ModelDataComp-package}
\alias{ModelDataComp-package}
\alias{ModelDataComp}
\docType{package}
\title{Model-Data Comparison}
\description{
Metrics and plots for model-data comparison. The package provides functions to 1) compute model performance metrics, to 2) multiple fit empirical models or regressions, and to 3) make model-data comparison plots.
}
\details{
\tabular{ll}{
Package: \tab ModelDataComp\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-11-17\cr
License: \tab GPL-2\cr
}

The package provides basic functions to compare different datasets or to compare models with data.

Model performance metrics: The function \code{\link{ObjFct}} computes several model performance metrics such as correlations, absolute and squared error metrics, bias metrics, and modelling efficiency metrics. The computed objective functions metrics can be converted to text strings with \code{\link{ObjFct2Text}} and can be plotted as barplots and scatterplots with \code{\link{plot.ObjFct}}.

Fit empirical models and regressions: The function \code{\link{MultiFit}} allows to fit multiple regression approaches between a response variable and one or several predictor variables. Currently considered regression approaches are linear regression, 2-nd and 3-rd order polynomial regression, smoothing splines, generalized additive models, random forest, and logistic functions. The function \code{\link{FitLogistic}} optimizes parameters of a regression based on additive or multiplicative logistic functions. 

Plotting: The package provides several plotting functions to compare model and data. \code{\link{ScatterPlot}} produces a scatter plot (with points, density lines, or point counts as image) and adds a fitting line based on one or several regression approaches as in \code{\link{MultiFit}}. \code{\link{ScatterPlot}} also allows to colour points and to fit regression lines for different groups. The function \code{\link{plot.ObjFct}} is the plotting routine for \code{\link{ObjFct}} objects. It allows to plot barplots and scatterplots of objective function metrics for several subsets of the model-data comparison. The function \code{\link{TaylorPlot}} produces a standard Taylor diagram for model-data comparison and is actually based on the \code{plotrix} package. The function \code{\link{WollMilchSauPlot}} allows to compare distributions (based on smoothed densities), mean values, and the performance or agreement between datasets and models in a single plot. \code{\link{WollMilchSauPlot}} extends the function \code{\link{pirateplot}} from the \code{yarrr} package by a colour gradient that is based on the performance or agreement of models with data. For example, ny model performance metric from \code{\link{ObjFct}} can be plotted as colour gradient in the pirateplot. 

}

\author{
Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]

Maintainer: Matthias Forkel <matthias.forkel@geo.tuwien.ac.at>
}

\keyword{ package }
\seealso{
greenbrown.r-forge.r-project.org
}


