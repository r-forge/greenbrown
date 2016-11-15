\name{text.ObjFct}
\alias{text.ObjFct}
\title{Convert objective functions to text}
\description{Convert objective functions to text.}
\usage{\method{text}{ObjFct}(x, which = c("Cor", "RMSE", "MEF"), sep = ", ", digits = 3)}
\arguments{
  \item{x}{an object of class \code{\link{ObjFct}}}
  \item{which}{which objective function metrics sshould be converted to text?}
  \item{sep}{separation between metrics}
  \item{digits}{digits for rounding}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{

obs <- rnorm(100) # "observations"
sim <- obs + rnorm(100, 0, 0.8) # simulation
of <- ObjFct(sim, obs)
of

# convert objective functions to text:
text(of)
text(of, which="KGE")
text(of, which=c("R2", "MEF", "KGE"), sep=" ")

}
