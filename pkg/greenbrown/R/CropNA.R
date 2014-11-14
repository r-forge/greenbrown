CropNA <- structure(function(
	##title<< 
	## Crop outer NA values from a raster
	##description<<
	## This function cuts NA values around an 'island' of real values in a Raster* object.

	r, 
	### Raster* object
	
	...
	#### other arguments. see writeRaster.
	
	##values<<
	## a Raster* object with smaller extent.
	) {
	# get cell number and coordinates from raster
	cell <- 1:ncell(r)
	xval <- xFromCell(r, cell)
	yval <- yFromCell(r, cell)

	# identify which cells have only NA values
	isna.r <- calc(r, function(x) {
		result <- 0
		if (all(is.na(x)) & AllEqual(x)) result <- 1
		return(result)
	})

	# select the cells that include not only NA values
	naval <- values(isna.r)
	r.df <- data.frame(xval, yval, naval)
	r.df <- subset(r.df, naval == 0)

	# create new extent 
	ext <- extent(min(r.df$xval), max(r.df$xval), min(r.df$yval), max(r.df$yval))

	# cropt raster
	r <- crop(r, ext, ...)
	return(r)
	### a Raster* object with smaller extent.
})
