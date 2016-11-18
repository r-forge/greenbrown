\name{plot.TrendSample}
\alias{plot.TrendSample}
\title{Plot uncertainty of estimated trend dependent on start and end dates of time series}
\description{Plotting function for objects of class \code{\link{TrendSample}}. The function plots a point scatter plot defined by first year (x-axis) and last year (y-axis) of the time series. For each combination of first and last year a point symbol is plotted that represents the estimated trend. The size of the point indicates the absolute value of the trend slope. The color of the point indicates the trend slope direction (blue = negative trend, red = positive trend). The symbol of the point indicates that p-value of the Mann-Kendall trend test (snowflake: p <= 0.05, cross: 0.05 < p <= 0.1, circle: p > 0.1). Additionally, a second plot is added to the main plot (only if full = TRUE). This second plot is a scatter plot of trend slope against p-value (Mann-Kendall trend test) using the same points symbols as in the main plot. Thus the second plot can serve as a legend for the symbols used in the main plot. A boxplot on top of the second plot shows the distribution of the trend slope.}
\usage{\method{plot}{TrendSample}(x, full = TRUE, response = "slope", ...)}
\arguments{
  \item{x}{objects of class \code{\link{TrendSample}}}
  \item{full}{make full plot or plot only main plot?}
  \item{response}{plot linear trend 'slope' or 'tau' from Mann-Kendall trend test as response variable.}
  \item{\dots}{further arguments to \code{\link{plot}}}
}



\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{TrendSample}}, \code{\link{TrendUncertainty}}		}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# calculate uncertainty of trend dependent on start and end dates
ndvi <- aggregate(ndvi, FUN=mean)
trd.ens <- TrendSample(ndvi)
trd.ens

# plot relations between start, end dates, length and trend statistics
plot(trd.ens)
plot(trd.ens, response="tau")

}
