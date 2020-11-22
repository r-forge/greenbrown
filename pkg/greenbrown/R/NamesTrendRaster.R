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
})
