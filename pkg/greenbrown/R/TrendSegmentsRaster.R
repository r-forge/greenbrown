TrendSegmentsRaster <- structure(function(
	##title<<
	## Identify for each multi-temporal raster layer the number of the trend segment
	
	##description<< Imagine you have a multi-temporal raster brick with 30 years of data. Now you compute trends using the function \code{\link{TrendRaster}}, which will return the timing of breakpoints as well as the slopes and p-values in each trend segment. But now you want to know for each pixel and each time step if it belongs to the first, second or Nth trend segment. For this you can use this function!

	trend.rb, 
	### multi-layer raster object of class \code{\link[raster]{brick}} as computed with \code{\link{TrendRaster}}
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	end=c(2011, 12), 
	### end of the time series (i.e. the time of the last observation). The default is c(2008, 12), i.e. December 2008 as the last observation
	
	freq=12,
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.
	
	min.length=0, 
	### Minimum duration of a trend in time steps of the input raster (see Details).
	
	max.pval=0.05,
	### Maximum p-value to classify a trend as being significant.
	
	...
	### additional arguments as for \code{\link{writeRaster}}
	
	##details<<
	## This function expects a RasterBrick as created with \code{\link{TrendRaster}} as input and assigns for each pixel and each time step the number of the trend segment. If a trend is not significant too short the time step will be flagged with NA. Per default a p-value of 0.05 is used to classify trends as significant. Additionally, the minimum duration of a trend can be specified with min.length: For example, if only time series segments longer than 10 years should be considered as trend, set min.length=11 in case of annual data. In case of monthly data set it to 132 (12 observations per year * 11 years)	
) {
	# get number of breakpoints from the length of the input raster
	breaks <- (nlayers(trend.rb) - 3) / 4	# calculate number of breaks from vector length

	# names of output vector from trend calculation
	names <- NamesTrendRaster(breaks)

	# create time vector
	time.ts <- ts(start=start, end=end, frequency=freq)
	time.ts <- format(floor(time(time.ts) * 10000), scientific=FALSE)

	.funForRaster <- function(x) {
		#x.bac <<- x
		if (AllEqual(x)) return(rep(NA, length(time.ts)))
				
		# get breakpoints
		bp.pos <- grep("BP", names) # get breakpoints from raster
		bps <- na.omit(x[bp.pos])

		# get pvalues
		pval.pos <- grep("Pval", names)
		pvals <- na.omit(x[pval.pos])

		# create vector of segments
		nseg <- length(bps) + 1	
		bps <- format(floor(bps * 10000), scientific=FALSE)
		seg.limits <- c(time.ts[1], bps, time.ts[length(time.ts)])	# create vector of segment limits

		result <- rep(NA, length(time.ts))
		for (s in 1:nseg) {
			# get start and end position of segments
			start.pos <- which(seg.limits[s] == time.ts)[1]
			start.pos[s > 1] <- start.pos + 1
			end.pos <- which(seg.limits[s+1] == time.ts)
			end.pos <- end.pos[length(end.pos)]
			
			# set segment ID in output vector if trend is significant
			if ((pvals[s] <= max.pval) & (length(start.pos:end.pos) > min.length)) {
				result[start.pos:end.pos] <- s
			} 
			names(result) <- time.ts
		}
		return(as.vector(result))
	}
	
	# apply function on raster
	trend.seg.rb <- calc(trend.rb, fun=.funForRaster, ...)
	names(trend.seg.rb) <- as.character(paste("X", time.ts, sep=""))
	return(trend.seg.rb)
	### The function returns a RasterBrick. 
},ex=function(){
# 
# # calculate trend
# trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=2)
# plot(trendmap)
# 
# # indicate for each time step the trend segment number
# trendsegmentsmap <- TrendSegmentsRaster(trendmap, min.length=5, max.pval=0.05, 
# 	start=c(1982, 1), end=c(2011, 1), freq=1)
# plot(trendsegmentsmap, 1:2, col=c("blue", "red")) 
# # first 2 years: everthing belongs to time series segment 1 
# plot(trendsegmentsmap, 29:30, col=c("blue", "red")) 
# # last 2 years: most pixel belong still to first time series segment 
# # (i.e. no breakpoints were detected), but some pixels are in the second 
# # time series segment (i.e. after the first breakpoint) 

})
