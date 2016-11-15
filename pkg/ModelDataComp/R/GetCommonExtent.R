GetCommonExtent <- structure(function(
	##title<< 
	## get the common extent of different rasterLayers
	##description<<
	## The function computes the minimum common extent of RasterLayers.

	raster.l
	### a list of rasterLayers
	) {

	# get extent of all rasters
	extent.l <- llply(raster.l, extent)
	xmin <- unlist(llply(extent.l, .fun=function(ext) { ext@xmin }))
	xmax <- unlist(llply(extent.l, .fun=function(ext) { ext@xmax }))
	ymin <- unlist(llply(extent.l, .fun=function(ext) { ext@ymin }))
	ymax <- unlist(llply(extent.l, .fun=function(ext) { ext@ymax }))
	
	ext <- extent(c(max(xmin), min(xmax), max(ymin), min(ymax)))
	return(ext)
	### The function returns an object of class extent
}, ex=function() {
#
})
