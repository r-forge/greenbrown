\name{CompareClassification}
\alias{CompareClassification}
\title{Compare two classification maps}
\description{This function computes an agreement map of two classifications (RasterLayers with classified values). Additionally, it computes a frequency table with user, producer and total accuracies as well as the \code{\link{Kappa}} coefficient. }
\usage{CompareClassification(x, 
    y, names = NULL, 
    samplefrac = 1)}
\arguments{
  \item{x}{First raster layer with classification.}
  \item{y}{Second raster layer with classification.}
  \item{names}{a list with \code{names} of the two classifications and class \code{names}. See example section for details.}
  \item{samplefrac}{fraction of grid cells to be sampled from both rasters in order to calculate the contingency table}
}

\value{The function returns a list of class "CompareClassification" with the following components:
\itemize{ 
\item{ \code{raster} a raster layer indicating the agreement of the two classifications. }
\item{ \code{table} a contingency table with user, producer and total accuracies. Rows in the table correpond to the classification \code{x}, columns to the classifcation \code{y}. }
\item{ \code{kappa} \code{\link{Kappa}} coefficient. }
}}

\author{Matthias Forkel <matthias.forkel@tu-dresden.de> [aut, cre]}



\seealso{\code{\link{plot.CompareClassification}}, \code{\link{AccuracyAssessment}}, \code{\link{TrendClassification}}}
\examples{
# Calculate NDVI trends from two methods and compare the significant trends

# # calculate trends with two different methods
# AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=0)
# plot(AATmap)
# STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=0)
# plot(STMmap)
# 
# # classify the trend estimates from the two methods into significant 
# # positive, negative and no trend
# AATmap.cl <- TrendClassification(AATmap)
# plot(AATmap.cl, col=brgr.colors(3))
# STMmap.cl <- TrendClassification(STMmap)
# plot(STMmap.cl, col=brgr.colors(3))
# 
# # compare the two classifications
# compare <- CompareClassification(x=AATmap.cl, y=STMmap.cl, 
#    names=list('AAT'=c("Br", "No", "Gr"), 'STM'=c("Br", "No", "Gr")))
# compare
# 
# # plot the comparison
# plot(compare)

}
