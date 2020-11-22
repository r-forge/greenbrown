brgr.colors <- structure(function(
	##title<< 
	## Brown-to-green color palette for NDVI trend maps
	##description<<
	## Positive trends in Normalized Difference Vegetation Index are called 'greening' whereas negative trends are called 'browning'. Creating maps of NDVI trends in these colors helps to read the map. This function provides a color scale from brown to green and can be used to plot NDVI trend maps.

	n 
	### Number of color levels

	##seealso<<
	## \code{\link{TrendRaster}}

) {
	.fun <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "forestgreen", "darkgreen"))
	col <- .fun(n)
	return(col)
	### A character vector of color names.
}, ex=function() {

cols <- brgr.colors(10)
brks <- seq(0, 1, 0.1)
plot(ndvimap, c(3, 6), col=cols, breaks=brks)

})


