MapBreakpoints <- structure(function(
	##title<< 
	## Plot map of breakpoints 
	##description<<
	## This function plots a map of breakpoints or adds breakpoints as points and text to map of trends.
	
	bp.r, 
	### raster layer with breakpoints as computed with \code{\link{TrendRaster}}.
	
	add=TRUE, 
	### add breakpoint map to actual map (default TRUE)
	
	add.text=TRUE, 
	### add text (i.e. year of breakpoint) to regional groups of breakpoints
	
	ntext = NULL,
	### number of regional groups of breakpoints that should be labelled with text
	
	breaks=NULL, 
	### class breaks to color breapoints (if NULL will be computed automatically)
	
	col=NULL, 
	### colors for breakpoints 
	
	cex=0.6, 
	### size of point symbols
	
	lwd=0.6, 
	### line width of  point symbols
	
	pch=1, 
	### type of point symbol 
	
	format.text,
	### format of the text if add.text=TRUE, default: %y
	
	...
	### further arguments for \code{\link{plot}}
	
	##seealso<<
	## \code{\link{TrendRaster}}, \code{\link{TrendSegmentsRaster}}
	
) {

	bp.sp <- rasterToPoints(bp.r)
	proj <- CRS(projection(bp.r))
	bp.sp0 <- SpatialPointsDataFrame(bp.sp[,1:2], data.frame(bp.sp[,3]), proj4string = proj)
	
	if (!hasArg(format.text)) format.text <- "%y"
	
	# compute class breaks for breakpoints
	if (is.null(breaks)) {
		breaks <- pretty(bp.sp[, -c(1:2)])
	}
	
	# colors for breakpoints
	if (is.null(col)) {
		require(RColorBrewer)
		.col <- colorRampPalette(brewer.pal(9, "RdPu")[4:9])
		col <- .col(length(breaks)-1)
	}
	cols <- col[findInterval(bp.sp[, -c(1:2)], breaks, all.inside=TRUE)]
	
	# simplify breakpoint layer too add text
	if (add.text) {
		if (is.null(ntext)) ntext <- round(nrow(bp.sp)*0.05, 0)
		if (ntext < 1) ntext <- 1
		bp.cluster <- kmeans(bp.sp[,1:2], ntext)
		bp.sp2 <- data.frame(bp.sp, cluster=bp.cluster$cluster)
		bp.sp3 <- aggregate(. ~ cluster, data=bp.sp2, FUN=median)[,-1]
		bp.sp4 <- SpatialPointsDataFrame(bp.sp3[,1:2], data.frame(bp.sp3[,3]), proj4string = proj)
		
		bp.txt <- as.Date(paste(round(bp.sp4@data[,1],0), "-01-01", sep=""))
		bp.txt <- format(bp.txt, format.text)
	}
	

	# add breakpoints as points
	if (!add) plot(bp.sp0, col=cols, cex=cex, lwd=lwd, pch=pch, ...)
	if (add) plot(bp.sp0, col=cols, add=TRUE, cex=cex, lwd=lwd, pch=pch, ...)
	
	# add breakpoints as text for regional clusters of breakpoints
	if (add.text) {
		cols.txt <- col[findInterval(bp.sp4@data[,1], breaks, all.inside=TRUE)]
		text(coordinates(bp.sp4)[,1], coordinates(bp.sp4)[,2], bp.txt, cex=cex*2, col=cols.txt)
	}
	return(list(col=cols, breaks=breaks, ntext=ntext))
	### The function returns a list with class colors and breaks that was used for plotting
}, ex=function() {
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
ndvimap
plot(ndvimap, 8)

# calculate trend and breakpoints
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=1)
plot(AATmap)

# plot trend slope and add breakpoints
bp.r <- raster(AATmap, grep("BP1", names(AATmap)))
plot(AATmap, grep("SlopeSEG1", names(AATmap)), col=brgr.colors(15))
MapBreakpoints(bp.r)

plot(AATmap, grep("SlopeSEG1", names(AATmap)), col=brgr.colors(15))
lgd <- MapBreakpoints(bp.r, format.text="%Y", ntext=10, cex=0.8)


})
	
