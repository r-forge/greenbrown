\name{KGE}
\alias{KGE}
\title{Compute Kling-Gupta efficiency and related metrics of two time series}
\description{This function is an implementation of the Kling-Gupta efficiency (KGE) (Gupta et al. 2009) for model evaluation. It was originally developed to compare modelled and observed time series. The KGE is a model evluation criterion that can be decomposed in the contribution of mean, variance and correlation on model performance. 
In this implemenation, the Kling-Gupta effciency is defined as following:
KGE = 1 - eTotal 
eTotal is the euclidean distance of the actual effects of mean, variance, correlation and trend (optional) on the time series:
eTotal = sqrt(eMean + eVar + eCor + eTrend) 
eTotal can be between 0 (perfect fit) and infinite (worst fit). }
\usage{KGE(x, ref, trend = FALSE, mosum.pval = 0.05, h = 0.15, breaks = 0, 
    eTrend.ifsignif = TRUE, ...)}
\arguments{
  \item{x}{time series from model result or factorial model run}
  \item{ref}{reference time series (observation or standard model run)}
  \item{trend}{Include the effect of trend in the calculation? (default: FALSE)}
  \item{mosum.pval}{(only used if trend=TRUE) See \code{\link{Trend}} for details.	}
  \item{h}{(only used if trend=TRUE) See \code{\link{Trend}} for details.	}
  \item{breaks}{(only used if trend=TRUE) See \code{\link{Trend}} for details.	}
  \item{eTrend.ifsignif}{compute effect on trend only if trend in reference series is significant, if FALSE compute always effect on trend (if trend = TRUE)}
  \item{\dots}{further arguments for the function \code{\link{Trend}}}
}

\value{The function returns a vector with the following components:
\itemize{ 
\item{ \code{KGE} Kling-Gupta effciency = 1 - eTotal }
\item{ \code{eTotal} total effect, i.e. euclidean distance }
\item{ \code{fMean} fraction of mean to the total effect}
\item{ \code{fVar} fraction of variance to the total effect }
\item{ \code{fCor} fraction of correlation to the total effect }
\item{ \code{fTrend} fraction of trend to the total effect (only if trend=TRUE)}
\item{ \code{eMean} effect of mean}
\item{ \code{eVar} effect of variance }
\item{ \code{eCor} effect of correlation  }
\item{ \code{eTrend} effect of trend (only if trend=TRUE)}	
}}
\references{Gupta, H. V., H. Kling, K. K. Yilmaz, G. F. Martinez (2009):  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology 377, 80-91.}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Trend}}}
\examples{
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# change the variance and compute effect
x <- ndvi + rnorm(length(ndvi), 0, 0.01)
plot(x, ndvi); abline(0,1)
KGE(x, ndvi, trend=FALSE)

# change mean and variance and compute effect
x <- ndvi + rnorm(length(ndvi), 0.02, 0.01)
plot(x, ndvi); abline(0,1)
KGE(x, ndvi, trend=FALSE)

# be careful when using trends and breakpoints 
# (using trends is not part of the original implementation of the Kling-Gupta efficiency):
KGE(x, ndvi, trend=TRUE, breaks=0)
KGE(x, ndvi, trend=TRUE, breaks=1)
	
}
