KGE <- structure(function(
	##title<< 
	## Compare two time series and calculate relative effects on mean, variance, correlation and trend (optional) based on Kling-Gupta efficiency
	##description<<
	## This function is an implementation of the Kling-Gupta efficiency (KGE) (Gupta et al. 2009) for model evaluation. It was originally developped to compare modelled and observed time series. The KGE is a model evluation criterion that can be decomposed in the contribution of mean, variance and correlation on model performance. 
	## In this implemenation, the Kling-Gupta effciency is defined as following:
	## KGE = 1 - eTotal 
	## eTotal is the euclidean distance of the actual effects of mean, variance, correlation and trend (optional) on the time series:
	## eTotal = sqrt(eMean + eVar + eCor + eTrend) 
	## eTotal can be between 0 (perfect fit) and infinite (worst fit). 
	
	x, 
	### time series from model result or factorial model run
	
	ref, 
	### reference time series (observation or standard model run)
	
	trend=FALSE, 
	### Include the effect of trend in the calculation? (default: FALSE)
	
	mosum.pval=0.05,
	### (only used if trend=TRUE) See \code{\link{Trend}} for details.	
	
	h=0.15, 
	### (only used if trend=TRUE) See \code{\link{Trend}} for details.	
	
	breaks=0, 
	### (only used if trend=TRUE) See \code{\link{Trend}} for details.	
	
	...
	### further arguments for the function \code{\link{Trend}}
	
	##details<<
	## ...
	
	##references<< 
	## Gupta, H. V., H. Kling, K. K. Yilmaz, G. F. Martinez (2009):  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology 377, 80-91.
	
	##seealso<<
	## \code{\link{Trend}}
	
	) {
	
	if (all(is.na(x)) | all(is.na(ref))) {
		result <- rep(NA, 10)
	} else {
		
		# effect on mean
		rMean <- mean(x, na.rm=TRUE) / mean(ref, na.rm=TRUE)
		eMean <- (rMean - 1)^2
		
		# effect on trend
		if (trend) {
			if (class(x) != "ts" & class(ref) != "ts") stop("Error in Effects: x and ref should be from class 'ts' if trend=TRUE.")
			freq <- frequency(x)
			# choose method for trend calculation
			method <- "AAT"
			if (freq > 1) method <- "STM"	
			
			# calculate trends
			if (!AllEqual(x)) {
				x.trd <- Trend(x, method=method, breaks=breaks, h=h, mosum.pval=mosum.pval)
				x.trd.seg <- TrendLongestSEG(x.trd) 
			} else {
				x.trd.seg <- c(NA, 0, 1, 1)
			}
			if (!AllEqual(ref)) {
				ref.trd <- Trend(ref, method=method, breaks=breaks, h=h, mosum.pval=mosum.pval)
				ref.trd.seg <- TrendLongestSEG(ref.trd) 
			} else {
				ref.trd.seg <- c(NA, 0, 1, 1)
			}	
			
			# check if trend of longest segment is signifcant
			if (ref.trd.seg[3] < 0.05) {
			
				# effect on trend if at least one is signifcant
				rTrend <- x.trd.seg[2] / ref.trd.seg[2]
				eTrend <- (rTrend - 1)^2
				
				# detrend series before calculation of other effects
				if (x.trd.seg[3] < 0.05) x <- x - x.trd$trend
				if (ref.trd.seg[3] < 0.05) ref <- ref - ref.trd$trend
				
			} else {	
				# effect on trend if both are not signifcant -> no effect
				eTrend <- 0
			}
		} else {
			eTrend <- NA
		}

		# effect on variance
		rVar <- sd(x, na.rm=TRUE) / sd(ref, na.rm=TRUE)
		eVar <- (rVar - 1)^2
		
		# effect on correlation
		d <- na.omit(cbind(x, ref))
		r <- cor(d[,1], d[,2])
		eCor <- (r - 1)^2	
		
		# fractional effects
		s <- sum(c(eMean, eTrend, eVar, eCor), na.rm=TRUE)
		fMean <- eMean / s
		fTrend <- eTrend / s
		fVar <- eVar / s
		fCor <- eCor / s
		
		# total effect
		eTotal <- sqrt(s)
		KGE <- 1 - eTotal
		
		result <- c(KGE, eTotal, fMean, fVar, fCor, fTrend, eMean, eVar, eCor, eTrend)
		result[is.nan(result)] <- NA
	}
	names(result) <- c("KGE", "eTotal", "fMean", "fVar", "fCor", "fTrend", "eMean", "eVar", "eCor", "eTrend")
	return(result)
	### The function returns a vector with the following components:
	### \itemize{ 
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
plot(x, ndvi)
KGE(x, ndvi, trend=FALSE)

# change mean and variance and compute effect
x <- ndvi + rnorm(length(ndvi), 0.02, 0.01)
plot(x, ndvi); abline(0,1)
KGE(x, ndvi, trend=FALSE)

# be careful when using trends and breakpoints (using trends is not part of the original implementation of the Kling-Gupta efficiency):
KGE(x, ndvi, trend=TRUE, breaks=0)
KGE(x, ndvi, trend=TRUE, breaks=1)
	
})

