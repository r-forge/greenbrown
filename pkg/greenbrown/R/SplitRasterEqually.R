SplitRasterEqually <- structure(function(
	##title<< 
	## Splits a raster in equal-area parts
	##description<<
	## This function splits a raster object in parts with ~ equal area. 
	
	data.r, 
	### raster, raster brick or raster stack.
	
	n
	### number of parts
) {
	# get total number of non NA grid cells
	mean.r <- calc(data.r, function(x) {
		s <- sum(!is.na(x))
		s[s == 0] <- NA
		return(s)
	})

	ntotal <- sum(na.omit(values(mean.r)))
	if (n > ncol(mean.r)/2) {
		n <- ncol(mean.r)/2
		warning(paste("SplitRasterEqually: n changed to", n))
	}

	# compute optimal splitting based on weighted quantile
	sp <- as(mean.r, "SpatialPointsDataFrame")
	df <- data.frame(lon=coordinates(sp)[,1], val=sp@data)
	require(Hmisc)	
	x.best <- wtd.quantile(df$lon, weights=df$val, seq(0, 1, length=n+1), na.rm=TRUE)
	
	# split raster according to optimal splitting
	tiles.l <- llply(as.list(1:(length(x.best)-1)), function(i) {
		xmin <- x.best[i]
		xmax <- x.best[i+1]
		tile.r <- crop(data.r, extent(xmin, xmax, extent(mean.r)@ymin, extent(mean.r)@ymax))
		return(tile.r)
	})
	
	return(tiles.l)
	### the function returns a list of raster layers
}, ex=function(){
# data(ndvimap)
# tiles.l <- SplitRasterEqually(ndvimap, n=4)
# tiles.l
})
