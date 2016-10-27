\name{MapBreakpoints}
\alias{MapBreakpoints}
\title{Plot map of breakpoints }
\description{This function plots a map of breakpoints or adds breakpoints as points and text to map of trends.}
\usage{MapBreakpoints(bp.r, add = TRUE, add.text = TRUE, ntext = NULL, 
    breaks = NULL, col = NULL, cex = 0.6, lwd = 0.6, pch = 1, 
    format.text, ...)}
\arguments{
  \item{bp.r}{raster layer with breakpoints as computed with \code{\link{TrendRaster}}.}
  \item{add}{add breakpoint map to actual map (default TRUE)}
  \item{add.text}{add text (i.e. year of breakpoint) to regional groups of breakpoints}
  \item{ntext}{number of regional groups of breakpoints that should be labelled with text}
  \item{breaks}{class breaks to color breapoints (if NULL will be computed automatically)}
  \item{col}{colors for breakpoints }
  \item{cex}{size of point symbols}
  \item{lwd}{line width of  point symbols}
  \item{pch}{type of point symbol }
  \item{format.text}{format of the text if add.text=TRUE, default: \%y}
  \item{\dots}{further arguments for \code{\link{plot}}}
}

\value{The function returns a list with class colors and breaks that was used for plotting}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{TrendRaster}}, \code{\link{TrendSegmentsRaster}}}
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
ndvimap
plot(ndvimap, 8)

# calculate trend and breakpoints
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=1)
plot(AATmap)

# plot trend slope and add breakpoints
bp.r <- raster(AATmap, grep("BP1", names(AATmap)))
plot(AATmap, grep("SlopeSEG1", names(AATmap)), col=brgr.colors(15))
MapBreakpoints(bp.r)

plot(AATmap, grep("SlopeSEG1", names(AATmap)), col=brgr.colors(15))
lgd <- MapBreakpoints(bp.r, format.text="\%Y", ntext=10, cex=0.8)


}
