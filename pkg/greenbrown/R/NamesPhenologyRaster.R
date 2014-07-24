NamesPhenologyRaster <- structure(function(
	##title<< 
	## Get the layer names for a PhenologyRaster raster brick
	##description<<
	## This function returns the layer names of a raster brick that was created using \code{\link{PhenologyRaster}}

	x, 
	### \code{RasterBrick} as created with \code{\link{PhenologyRaster}} or \code{integer} as the number of years of the input data when the function \code{\link{PhenologyRaster}} was called.
	
	start=NULL
	### beginning of the time series.	

	##seealso<<
	## \code{\link{PhenologyRaster}}
) {
	if (class(x) == "RasterBrick") {
		nyears <- nlayers(x) / 10 # 10 number of output variables
	} else if (is.vector(x)) {
		nyears <- x
	}
	if (is.null(start)) start <- 1
	time <- seq(start[1], length=nyears, by=1)
	names <- c(paste("SOS", time, sep="."), paste("EOS", time, sep="."), paste("LOS", time, sep="."), paste("POP", time, sep="."), paste("MGS", time, sep="."), paste("PEAK", time, sep="."), paste("MSP", time, sep="."), paste("MAU", time, sep="."), paste("RSP", time, sep="."), paste("RAU", time, sep="."))
	return(names)
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate trend with maximum 2 breakpoints
phenmap <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12)
plot(phenmap)
plot(phenmap)

# layer names of the result
NamesPhenologyRaster(30)
NamesPhenologyRaster(phenmap)
NamesPhenologyRaster(phenmap, start=1982)
names(phenmap)


})

