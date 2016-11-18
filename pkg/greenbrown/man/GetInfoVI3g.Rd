\name{GetInfoVI3g}
\alias{GetInfoVI3g}
\title{Get meta-information from GIMMS VI3g binary file names}
\description{This function extracts the date and satellite from the GIMMS VI3g file names.}
\usage{GetInfoVI3g(file)}
\arguments{
  \item{file}{GIMMS VI3g file name}
}

\value{The function returns a list with $date and $sat.}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}




\examples{
# GetInfoVI3g("geo00oct15a.n14-VI3g")
}
