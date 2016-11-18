\name{PhenopixMY}
\alias{PhenopixMY}
\title{Multi-year phenology analysis using phenopix}
\description{This function takes a multi-year time series and applies curve fitting and phenology extraction functions based on the \code{\link{greenProcess}} function in the \code{phenopix} package. The function returns an object of class \code{PhenopixMY} (phenopix multi-year) which contains a list of \code{phenopix} objects. \code{PhenopixMY} can be plotted using \code{\link{plot.PhenopixMY}}.}
\usage{PhenopixMY(ts, fit, threshold = NULL, plot = FALSE, ...)}
\arguments{
  \item{ts}{a time series of class 'ts' or 'zoo' with multiple years of data}
  \item{fit}{fitting function to be applied, available options are: spline, beck, elmore, klosterman, gu (see \code{\link{greenProcess}})}
  \item{threshold}{threshold to be applied to compute phenology metrics, available options are: trs, derivatives, klosterman, gu (see \code{\link{greenProcess}}) }
  \item{plot}{plot phenopix object of each year, using \code{\link{plot.phenopix}}}
  \item{\dots}{further arguments as in \code{\link{greenProcess}}}
}

\value{An object of class \code{phenopixmy} with dedicated functions: plot(), print(). The structure is actually a list.}
\references{Filippa, G., Cremonese, E., Migliavacca, M., Galvagno, M., Forkel, M., Wingate, L., Tomelleri, E., Morra di Cella, U. and Richardson, A. D.: Phenopix: A R package for image-based vegetation phenology, Agricultural and Forest Meteorology, 220, 141-150, doi:10.1016/j.agrformet.2016.01.006, 2016.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{greenProcess}}, \code{\link{plot.PhenopixMY}}, \code{\link{Phenology}} }
\examples{

data(ndvi)
plot(ndvi)

ppixmy <- PhenopixMY(ndvi, "spline", "trs")
plot(ppixmy)

plot(ppixmy, type="metrics")

}
