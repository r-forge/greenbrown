\name{ObjFct2Text}
\alias{ObjFct2Text}
\title{Convert objective functions to text}
\description{Converts an object of class \code{\link{ObjFct}} to text string.}
\usage{ObjFct2Text(x, which = c("Cor", "RMSE", "MEF"), sep = ", ", digits = 2)}
\arguments{
  \item{x}{an object of class \code{\link{ObjFct}}}
  \item{which}{which objective function metrics should be written as text?}
  \item{sep}{separation between metrics}
  \item{digits}{digits for rounding}
}
\details{Converts an object of class 'ObjFct' to text string.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{

obs <- rnorm(100) # "observations"
sim <- obs + rnorm(100, 0, 0.8) # simulation
of <- ObjFct(sim, obs)
of

# convert objective functions to text:
ObjFct2Text(of)
ObjFct2Text(of, which="KGE")
ObjFct2Text(of, which=c("R2", "MEF", "KGE"), sep=" ")

}
