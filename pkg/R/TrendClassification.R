TrendClassification <- structure(function(
	##title<< 
	## Classify a raster in greening and browning trends
	##description<<
	## This function classifies a RasterBrick with trend estimates as computed with \code{\link{TrendRaster}} into positive, negative and no trend per each time series segment.

	r, 
	### multi-layer raster object of class \code{\link[raster]{brick}} as computed with \code{\link{TrendRaster}}
	
	min.length=0, 
	### Minimum duration of a trend in time steps of the input raster (see Details).
	
	max.pval=0.05,
	### Maximum p-value to classify a trend as being significant.
	
	...
	### additional arguments as for \code{\link{writeRaster}}
		
	##details<<
	## This function expects a RasterBrick as created with \code{\link{TrendRaster}} as input and classifies for each pixel and each time series segment if a trend is significant positive, significant negative or not significant (no trend). Per default a p-value of 0.05 is used to classify trends as significant. Additionally, the minimum duration of a trend can be specified with min.length: For example, if only time series segments longer than 10 years should be considered as trend, set min.length=11 in case of annual data. In case of monthly data set it to 132 (12 observations per year * 11 years).

	##seealso<<
	## \code{\link{TrendRaster}}

	) {

	# get number of breakpoints from the length of the vector
	breaks <- (nlayers(r) - 3) / 4	# calculate number of breaks from vector length
	nseg <- breaks + 1
		
	.funForRaster <- function(x) {
		# get slope and pvalue for segment
		result <- rep(NA, nseg)
		if (!all(is.na(x))) {
			# get pvalues and slopes
			for (s in 1:nseg) {
				pval <- x[nseg+breaks+nseg+s]
				slope <- x[nseg+breaks+s]
				seg.length <- x[s]
				if (!is.na(pval) & !is.na(slope)) {
					result[s] <- 0	# no trend
					if ((slope > 0) & (pval <= max.pval)) result[s] <- 1	# significant positive trend (greening)
					if ((slope < 0) & (pval <= max.pval)) result[s] <- -1	# significant negative trend (browning)
					if (seg.length < min.length) result[s] <- 0	# no trend if segment is too short	
				}
			}
		}
		return(result)
	}
	trend <- calc(r, .funForRaster, ...)
	layerNames(trend) <- paste("TrendSEG", 1:nseg, sep="")
	return(trend)
	### The function returns a RasterLayer in case of one time series segment or a RasterBrick in case of multiple time series segments. Pixels with a significant positive trend have the value 1; pixels with significant negative trends -1 and non-significant trends 0.
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
ndvimap
plot(ndvimap, 8)

# calculate trends on the raster
trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=2)
plot(trendmap)

# classify the trends in greening and browning
greenbrownmap <- TrendClassification(trendmap, min.length=10, max.pval=0.05)
plot(greenbrownmap, col=brgr.colors(3))

})


