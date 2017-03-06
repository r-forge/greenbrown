TrendSample <- structure(function(
	##title<< 
	## Compute trend statistics by sampling a time series according to different start and end dates
	
	##description<<
	## The function computes an ensemble of trend statistics (linear trend slope, Mann-Kendall tau and p-value) on a time series by sampling different start and end dates of the time series. This ensemble can be used to compute uncertainties in trend statistics. Results can be plotted using the function \code{\link{plot.TrendSample}}.
	
	Yt,
	### univariate time series of class \code{\link{ts}}
		
	sample.method = c("all", "sample", "none"),
	### Sampling method for combinations of start and end dates to compute uncertainties in trends. If "sample" (default), trend statistics are computed for a sample of combinations of start and end dates according to \code{sample.size}. If "all", trend statistics are computed for all combinations of start and end dates longer than \code{sample.min.length}.  If "none", trend statistics will be only computed for the entire time series (i.e. no sampling of different start and end dates). 
	
	sample.min.length = 0.7,
	### Minimum length of the time series (as a fraction of total length) that should be used to compute trend statistics. Time windows between start and end that are shorter than min.length will be not used for trend computation.
	
	sample.size = 30,
	### sample size (number of combinations of start and end dates) to be used if \code{method} is sample.
	
	trend = TrendAAT
	### method that should be used to compute the trend
	
	##references<<  
		
	##seealso<<
	## \code{\link{Trend}}, \code{\link{plot.TrendSample}}
) {

	time <- time(Yt)
	years <- unique(round(time, 0))
	nyears <- length(years)
	sample.method <- sample.method[1]
	
	if (sum(!is.na(Yt)) < 5 | length(Yt) < 3) {
		stats <- data.frame(start=NA, end=NA, length=NA, tau=NA, pvalue=NA, slope=NA, intercept=NA, perc=NA)
		class(stats) <- "TrendSample"
		return(stats)
	}
	
	# minimum length 
	min.length <- nyears * sample.min.length
	if (min.length * frequency(Yt) < 4) sample.method <- "none"

	# get all combinations of start and end days
	samples0 <- data.frame(start=years[1], end=years[nyears], length=nyears)
	if (sample.method == "none") {
		samples <- samples0
	} else {
		if (nyears > 1000) {
			samples <- expand.grid(years[1:300], years[(nyears-300):nyears])
		} else {
			samples <- expand.grid(years, years)
		}
		samples[,3] <- samples[,2] - samples[,1] + 1
		samples <- samples[samples[,3] > min.length, ]
		colnames(samples) <- c("start", "end", "length")
		if (sample.method == "sample") {
		   if (sample.size < nrow(samples)) samples <- rbind(samples0, samples[sample(1:nrow(samples), sample.size-1), ])
	   } else {
	      full <- which.max(samples$length)
	      samples <- rbind(samples[full, ], samples[-full, ])
	   }
	}

	# compute trend statistics for each sample
	stats <- ldply(as.list(1:nrow(samples)), function(i) {
		x <- unlist(samples[i ,])
		if (frequency(Yt) > 1) {
			Yt.sample <- window(Yt, start=c(x[1], 1), end=c(x[2], frequency(Yt)))
		} else {
			Yt.sample <- window(Yt, start=x[1], end=x[2])
		}
			
		if (length(Yt.sample) < 3) {
			result <- data.frame(start=x[1], end=x[2], length=x[3], slope=NA, slope_se=NA, pval=NA, perc=NA, mk.tau=NA, mk.pval=NA)
		} else {
			trd <- do.call(trend, list(Yt=Yt.sample, breaks=0))
			result <- data.frame(start=x[1], end=x[2], length=x[3], slope=trd$slope, slope_se=trd$slope_se, pval=trd$pval, perc=trd$perc, mk.tau=trd$mk.tau, mk.pval=trd$mk.pval)
		}
		return(result)
	})
	stats <- stats[order(stats$length, decreasing=TRUE), ]
	rownames(stats) <- 1:nrow(stats)
	class(stats) <- "TrendSample"
	
	suppressWarnings(stats$tau.test <- wilcox.test(stats$mk.tau))
	suppressWarnings(stats$slope.test <- wilcox.test(stats$slope))
	
	return(stats)
	### The function returns a data.frame with the start date, end date and length of the sample from the time series and the correspondig Mann-Kendall tau, p-value, slope, intercept, and percentage slope of a linear trend.
}, ex=function(){
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)

# calculate uncertainty of trend dependent on start and end dates
trd.ens <- TrendSample(ndvi, trend=TrendAAT)
plot(trd.ens)
plot(trd.ens, "tau")

trd.ens <- TrendSample(ndvi, trend=TrendRQ)
plot(trd.ens)


# plot relations between start, end dates, length and trend statistics



})

