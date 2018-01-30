\name{Col2}
\alias{Col2}
\title{Adds transparency to a colour}
\description{The function adds transparency to a vector of colours}
\usage{Col2(col, alpha = 0.3, ...)}
\arguments{
  \item{col}{character string of the colour}
  \item{alpha}{transparence (0 = transparent, 1 = opaque)}
  \item{\dots}{further arguments (unused)}
}
\details{The function adds transparency to a vector of colours}
\value{returns a colour including transparency}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{
cols <- Col2(c("red", "blue"), alpha=seq(0, 1, 0.1))
plot.new()
legend("top", cols, fill=cols, bg="yellow")
}
