NamesTrendRaster <- structure(function(
	##title<< 
	## Get the layer names for a TrendRaster raster brick
	##description<<
	## This function returns the layer names of a raster brick that was created using \code{\link{TrendRaster}}

	x 
	### \code{RasterBrick} as created with \code{\link{TrendRaster}} or \code{integer} as the maximum number of breakpoints that was used when the function \code{\link{TrendRaster}} was called.

	##seealso<<
	## \code{\link{TrendRaster}}
) {
	if (class(x) == "RasterBrick") {
		breaks <- (nlayers(x) - 3) / 4
	} else if (is.vector(x)) {
		breaks <- x
	}

	if (breaks > 0) names <- c(paste("LengthSEG", 1:(breaks+1), sep=""), paste("BP", 1:breaks, sep=""), paste("SlopeSEG", 1:(breaks+1), sep=""), paste("PvalSEG", 1:(breaks+1), sep=""))
	if (breaks == 0) names <- c(paste("LengthSEG", 1:(breaks+1), sep=""), paste("SlopeSEG", 1:(breaks+1), sep=""), paste("PvalSEG", 1:(breaks+1), sep=""))
	return(names)
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate trend with maximum 2 breakpoints
breaks <- 2
trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=breaks)
plot(trendmap)

# layer names ot the result
NamesTrendRaster(breaks)
NamesTrendRaster(trendmap)
layerNames(trendmap)

# now imagine you are loosing the layer names ...
layerNames(trendmap) <- 1:11
plot(trendmap)	# X1, X2 ... is not meaningfull. How can you get the names back?

# re-create the layer names with NamesTrendRaster
layerNames(trendmap) <- NamesTrendRaster(trendmap)
plot(trendmap)

})

