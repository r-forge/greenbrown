KGERaster <- structure(function(
	##title<< 
	## Compute Kling-Gupta efficiency and related metrics of two multi-layer raster data sets
	##description<<
	## This function can be used to apply the function \code{\link{KGE}} on raster data. See \code{\link{KGE}} for details.
	
	x, 
	### multi-layer raster object of class \code{\link[raster]{brick}} including modelled time series
	
	ref, 
	### multi-layer raster object of class \code{\link[raster]{brick}} including reference (observed or standard model run) time series
	
	trend=FALSE, 
	### Include the effect of trend in the calculation? (default: FALSE). The calculation of breakpoints is currently not implemented for the function KGERaster.
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12,
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.
		
	...
	### further arguments for the function \code{\link{calc}}
	
	##details<<
	## See \code{\link{KGE}} for details.
	
	##references<< 
	## Gupta, H. V., H. Kling, K. K. Yilmaz, G. F. Martinez (2009):  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology 377, 80-91.
	
	##seealso<<
	## \code{\link{KGE}}, \code{\link{Trend}}
	
	) {
		
	if (trend) {
		# choose method for trend calculation
		method <- "AAT"
		if (freq > 1) method <- "STM"	
		x.trd <- TrendRaster(x, start=start, freq=freq, breaks=0, method=method)
		ref.trd <- TrendRaster(ref, start=start, freq=freq, breaks=0, method=method)
		
		# get slopes
		x.slope <- raster(x.trd, 2)
		x.pval <- raster(x.trd, 3)
		ref.slope <- raster(ref.trd, 2)
		ref.pval <- raster(ref.trd, 3)
		
		# effect on trend
		rTrend <- x.slope / ref.slope
		rTrend[x.pval > 0.05 & ref.pval > 0.05] <- 1
		eTrend <- (rTrend - 1)^2	
	} else {
		eTrend <- raster(x, 1)
		eTrend[] <- 0
	}
	
	# effect on mean
	x.mean <- mean(x, na.rm=TRUE)
	ref.mean <- mean(ref, na.rm=TRUE)
	rMean <- x.mean / ref.mean
	eMean <- (rMean - 1)^2
	
	# NA mask
	mask.r <- x.mean + ref.mean
	
	# detrend series in case of trend
	if (trend) {
		# create raster brick with all time steps
		timesteps <- x
		for (i in 1:nlayers(x)) timesteps[[i]] <- i
		
		# detrend
		x <- x - (x.slope * timesteps)
		ref <- ref - (ref.slope * timesteps)
	}
	
	# effect on variance
	x.sd <- calc(x, sd, na.rm=TRUE)
	ref.sd <- calc(ref, sd, na.rm=TRUE)
	rVar <- x.sd / ref.sd
	eVar <- (rVar - 1)^2
	
	# calculate deviations
	x.dev <- x - x.mean
	ref.dev <- ref - ref.mean
	
	# effect on correlation
	a <- sum(x.dev * ref.dev, na.rm=TRUE)
	b <- sqrt(sum(x.dev^2, na.rm=TRUE)) * sqrt(sum(ref.dev^2, na.rm=TRUE))
	r <- a / b
	eCor <- (r - 1)^2	
	
	# fractional effects
	s <- eMean + eTrend + eVar + eCor
	fMean <- eMean / s
	fTrend <- eTrend / s
	fVar <- eVar / s
	fCor <- eCor / s
	
	# if (!trend) {
		# eTrend[] <- NA
		# fTrend[] <- NA
	# }
	
	# total effect
	eTotal <- sqrt(s)
	KGE <- 1 - eTotal
	
	kge.r <- brick(stack(KGE, eTotal, fMean, fVar, fCor, fTrend, eMean, eVar, eCor, eTrend))
	kge.r <- raster::mask(kge.r, mask.r)
	names(kge.r) <- c("KGE", "eTotal", "fMean", "fVar", "fCor", "fTrend", "eMean", "eVar", "eCor", "eTrend")
	return(kge.r)
	### The function returns a raster brick with the following layers:
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
# # load a map of NDVI (normalized difference vegetation index) time series
# data(ndvimap)
# plot(ndvimap)

# # increase mean
# ndvimap2 <- ndvimap + 0.01
# kge1.r <- KGERaster(x=ndvimap2, ref=ndvimap)
# plot(kge1.r)

# # increase mean and variance
# ndvimap3 <- ndvimap + 0.01 + rnorm(1000, 0, 0.05)
# kge2.r <- KGERaster(ndvimap3, ndvimap)
# plot(kge2.r)

# # check also effects on trend (takes more time because of trend calculations)
# kge3.r <- KGERaster(ndvimap3, ndvimap, trend=TRUE)
# plot(kge3.r)
	
})

