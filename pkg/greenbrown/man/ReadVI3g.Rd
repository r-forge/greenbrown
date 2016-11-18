\name{ReadVI3g}
\alias{ReadVI3g}
\title{Read and pre-process GIMMS VI3g binary files}
\description{This function reads GIMMS VI3g binary files, pre-processes the values (exclusion of flagged values, subset for area of interest) and returns the result as a raster layer. The function can be used to read the original GIMMS NDVI3g data files to R.}
\usage{ReadVI3g(file, flag = 2:7, ext = c(-180, 180, -90, 90))}
\arguments{
  \item{file}{GIMMS VI3g file name}
  \item{flag}{vector of quality flags that should be excluded. Default: 2:7 (all values with reduced quality excluded). If you want to keep all values set \code{flag=NA}.}
  \item{ext}{extent (xmin, xmax, ymin, ymax) for which to extract the data}
}
\details{The GIMMS NDVI3g dataset comes with the following quality flags:	
\itemize{ 
\item{ FLAG = 1 (Good value) }
\item{ FLAG = 2 (Good value, possibly snow) }
\item{ FLAG = 3 (NDVI retrieved from spline interpolation) }
\item{ FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow) }
\item{ FLAG = 5 (NDVI retrieved from average seasonal profile)}
\item{ FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow) }
\item{ FLAG = 7 (missing data) }
}	}
\value{raster layer }

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{
#	data <- ReadVI3g("geo00oct15a.n14-VI3g")
#	plot(data)
}
