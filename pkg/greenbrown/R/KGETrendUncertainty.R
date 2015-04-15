KGETrendUncertainty <- structure(function(
	##title<< 
	## Compute uncertainty of Kling-Gupta efficiency based on beginning and end of time series 
	##description<<
	## This function samples time series for different combinations of start and end years and computes for each combination the KGE (see \code{\link{KGE}}).
	
	x, 
	### time series from model result or factorial model run
	
	ref, 
	### reference time series (observation or standard model run)
	
	trend=TRUE, 
	### Include the effect of trend in the calculation? 
	
	eTrend.ifsignif = FALSE,
	### compute effect on trend only if trend in reference series is significant, if FALSE compute always effect on trend (if trend = TRUE)
	
	sample.method = c("sample", "all", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.75,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30,
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.
		
	...
	### further arguments for the function \code{\link{Trend}}
	
	##details<<
	## ...
	
	##references<< 
	## Gupta, H. V., H. Kling, K. K. Yilmaz, G. F. Martinez (2009):  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology 377, 80-91.
	
	##seealso<<
	## \code{\link{Trend}}
	
	) {
	
	d <- ts.intersect(x, ref)
	x <- d[,1]
	ref <- d[,2]
	Yt <- ref
	
	n <- length(Yt)
	time <- time(Yt)
	sample.method <- sample.method[1]
        
	# minimum length 
	min.length <- n / frequency(Yt) * sample.min.length
	if (min.length * frequency(Yt) < 4) sample.method <- "none"

	# get all combinations of start and end days
	samples0 <- data.frame(start=time[1], end=time[n], length=n / frequency(Yt))
	if (sample.method == "none") {
		samples <- samples0
	} else {
		if (n > 1000) {
			samples <- expand.grid(time[1:300], time[(n-300):n])
		} else {
			samples <- expand.grid(time, time)
		}
		samples[,3] <- samples[,2] - samples[,1] + 1
		samples <- samples[samples[,3] > min.length, ]
		colnames(samples) <- c("start", "end", "length")
	}
	if (sample.method == "sample") {
		if (sample.size < nrow(samples)) samples <- rbind(samples0, samples[sample(1:nrow(samples), sample.size-1), ])
	}
	
	# calculate KGE for each samples
	kge0 <- KGE(1:10, 1:10)
	stats <- ldply(as.list(1:nrow(samples)), function(i) {
		s <- unlist(samples[i ,])
		x.sample <- window(x, start=s[1], end=s[2])
		ref.sample <- window(ref, start=s[1], end=s[2])
		if (length(x.sample) < 3) {
			result <- data.frame(start=s[1], end=s[2], length=s[3], KGE=NA, eTotal=NA, fMean=NA, fVar=NA, fCor=NA, fTrend=NA, eMean=NA, eVar=NA, eCor=NA, eTrend=NA)
		} else {
			kge <- KGE(x.sample, ref.sample, trend=trend, mosum.pval=0.05, h=0.15, breaks=0, eTrend.ifsignif=eTrend.ifsignif)
			result <- data.frame(start=s[1], end=s[2], length=s[3], KGE=kge[1], eTotal=kge[2], fMean=kge[3], fVar=kge[4], fCor=kge[5], fTrend=kge[6], eMean=kge[7], eVar=kge[8], eCor=kge[9], eTrend=kge[10])
		}
		return(result)
	})
	stats <- stats[order(stats$length, decreasing=TRUE), ]
	rownames(stats) <- 1:nrow(stats)
	class(stats) <- "KGETrendUncertainty"
	
	# names(stats) <- c("start", "end", "length", "KGE", "eTotal", "fMean", "fVar", "fCor", "fTrend", "eMean", "eVar", "eCor", "eTrend")
	return(stats)
	### The function returns a data.frame with the following components:
	### \itemize{ 
	### \item{ \code{start} start of the time series }
	### \item{ \code{end} end of the time series }
	### \item{ \code{length} length of the time series }
	### \item{ \code{KGE} Kling-Gupta effciency = 1 - eTotal }
	### \item{ \code{eTotal} total effect, i.e. euclidean distance }
	### \item{ \code{fMean} fraction of mean to the total effect}
	### \item{ \code{fVar} fraction of variance to the total effect }
	### \item{ \code{fCor} fraction of correlation to the total effect }
	### \item{ \code{fTrend} fraction of trend to the total effect (only if trend=TRUE)}
	### \item{ \code{eMean} effect of mean}
	### \item{ \code{eVar} effect of variance }
	### \item{ \code{eCor} effect of correlation  }
	### \item{ \code{eTrend} effect of trend (only if trend=TRUE)}	
	### }
},ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# change the variance and compute effect
x <- ndvi + rnorm(length(ndvi), 0, 0.01)
plot(x, ndvi); abline(0,1)
unc <- KGETrendUncertainty(x, ndvi)
hist(unc$KGE)
	
})

