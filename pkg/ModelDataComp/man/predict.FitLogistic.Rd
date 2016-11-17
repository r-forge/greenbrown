\name{predict.FitLogistic}
\alias{predict.FitLogistic}
\title{Predict method for logistic function fits}
\description{Predict values based on logistic function fits.}
\usage{\method{predict}{FitLogistic}(object, newdata, ...)}
\arguments{
  \item{object}{an oject as returned by \code{\link{FitLogistic}}}
  \item{newdata}{data.frame of predictor variables}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}
\value{a vector of predicted values}
\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{FitLogistic}}}
\examples{

x <- 1:1000
y <- Logistic(c(1, 0.01, 500), x) + rnorm(1000, 0, 0.01)
plot(x, y)
fit <- FitLogistic(x, y)
lines(x, fit$predicted, col="red")

newdata <- data.frame(x=-500:1000)
pred <- predict(fit, newdata)
plot(newdata$x, pred)

}
