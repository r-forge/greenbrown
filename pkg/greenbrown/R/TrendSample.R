TrendSample <- structure(function(
	##title<< 
	## Compute trend statistics by sampling a time series according to different start and end dates
	
	##description<<
	## The function computes an ensemble of trend statistics (linear trend slope, Mann-Kendall tau and p-value) on a time series by sampling different start and end dates of the time series. This ensemble can be used to compute uncertainties in trend statistics. 
	
	Yt,
	### univariate time series of class \code{\link{ts}}
		
	sample.method = c("sample", "all", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.75,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.
	
	##references<<  
		
	##seealso<<
	## \code{\link{Trend}}
) {

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
		samples <- expand.grid(time, time)
		samples[,3] <- samples[,2] - samples[,1] + 1
		samples <- samples[samples[,3] > min.length, ]
		colnames(samples) <- c("start", "end", "length")
	}
	if (sample.method == "sample") {
		if (sample.size < nrow(samples)) samples <- rbind(samples0, samples[sample(1:nrow(samples), sample.size-1), ])
	}
	
	# compute trend statistics for each sampe
	stats <- ldply(as.list(1:nrow(samples)), function(i) {
		x <- unlist(samples[i ,])
		Yt.sample <- window(Yt, start=x[1], end=x[2])
		if (length(Yt.sample) < 3) {
			result <- data.frame(start=x[1], end=x[2], length=x[3], tau=NA, pvalue=NA, slope=NA, intercept=NA)
		} else {
			time.sample <- time(Yt.sample)
			mk <- MannKendall(Yt.sample)
			m <- lm(Yt.sample ~ time.sample)
			result <- data.frame(start=x[1], end=x[2], length=x[3], tau=mk$tau, pvalue=mk$sl, slope=coef(m)[2], intercept=coef(m)[1])
		}
		return(result)
	})
	stats <- stats[order(stats$length, decreasing=TRUE), ]
	rownames(stats) <- 1:nrow(stats)
	return(stats)
	### The function returns a data.frame with the start date, end date and length of the sample from the time series and the correspondig Mann-Kendall tau, p-value and the slope and intercept of a linear trend.
},ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# calculate uncertainty of trend dependent on start and end dates
ndvi <- aggregate(ndvi, FUN=mean)
trd.ens <- TrendSample(ndvi)
trd.ens

# plot relations between start, end dates, length and trend statistics
plot(trd.ens)

# plot time series and trend lines
plot(ndvi)
for (i in nrow(trd.ens):1) {
	trd <- trd.ens$slope[i] * c(trd.ens$start[i], trd.ens$end[i]) + trd.ens$intercept[i]
	if (i == 1) segments(trd.ens$start[i], trd[1], trd.ens$end[i], trd[2], col="red", lwd=2)
	segments(trd.ens$start[i], trd[1], trd.ens$end[i], trd[2], col="blue")
}

})



