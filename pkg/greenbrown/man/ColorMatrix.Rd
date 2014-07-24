\name{ColorMatrix}
\alias{ColorMatrix}
\title{Create a square matrix of colors}
\description{This function creates a square matrix with two diagonal crossing color ramps. It can be used to plot contingency maps of two classifications.}
\usage{ColorMatrix(dim = 3, ul = "burlywood4", lr = "darkgreen", ll = "khaki1", 
    ur = "royalblue1", ctr = "gray87")}
\arguments{
  \item{dim}{number of rows and number of columns of the matrix (only square matrix are possible, i.e. number of rows = number columns)}
  \item{ul}{starting color in the upper left corner of the matrix}
  \item{lr}{ending color in the lower right corner of the matrix	}
  \item{ll}{starting color in the lower left corner of the matrix}
  \item{ur}{ending color in the upper right corner of the matrix	}
  \item{ctr}{color in the center of the matrix}
}

\value{The function returns a square matrix of color names. }

\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}




\examples{
col.m <- ColorMatrix()
plot.new()
legend("topleft", as.vector(col.m), fill=col.m, ncol=3)

col.m <- ColorMatrix(dim=5, ul="red", ll="navy", ctr="purple")
plot.new()
legend("topleft", as.vector(col.m), fill=col.m, ncol=5)
}
