plot.CompareClassification <- structure(function(
	##title<< 
	## plot a comparison of two classification rasters
	
	##description<<
	## This function takes an object of class \code{\link{CompareClassification}} as input and plots a map of the class agreement of two classifications.
	
	x,
	### Object of class 'CompareClassification' as returned from function \code{\link{CompareClassification}}.
	
	xlab="",
	### A title for the x axis
	
	ylab="",
	### A title for the y axis
	
	main="Classification agreement",
	### A title for the plot
	
	ul="burlywood4", 
	### starting color in the upper left corner of the \code{\link{ColorMatrix}}
	
	lr="darkgreen", 
	### ending color in the lower right corner of the \code{\link{ColorMatrix}}
	
	ll="khaki1", 
	### starting color in the lower left corner of the \code{\link{ColorMatrix}}
	
	ur="royalblue1", 
	### ending color in the upper right corner of the \code{\link{ColorMatrix}}	
	
	ctr="gray87",
	### color in the center of the \code{\link{ColorMatrix}}	
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{CompareClassification}}, \code{\link{AccuracyAssessment}}, \code{\link{TrendClassification}}
) {

	# number of classes
	ncl <- nrow(x$table) - 2
	cl <- 1:(ncl*ncl)

	# create colors for map
	col.m <- ColorMatrix(ncl, ul, lr, ll, ur, ctr)

	# create legend text
	agree <- round(as.vector(prop.table(x$table[1:ncl, 1:ncl]) * 100), 1) 
	lgd <- expand.grid(rownames(x$table)[1:ncl], rownames(x$table)[1:ncl])
	lgd <- paste(lgd[,1], "/", lgd[,2], " = ", agree, sep="")	
	
	# create plot
	par(mar=c(2.7, 2.7, ncl*2, 2))
	brks <- seq(min(cl)-0.5, max(cl)+0.5)
	plot(x$raster, col=as.vector(col.m), breaks=brks, xlab=xlab, ylab=ylab)
	coord <- coordinates(x$raster)
	legend(x=mean(coord[,1]), y=max(coord[,2])+(abs(max(coord[,2]) - min(coord[,2])) * 0.02), lgd, fill=as.vector(col.m), ncol=ncl, xjust=0.5, yjust=0, xpd=TRUE, bty="n", title=main)
}, ex=function() {
# Example: calculate NDVI trends from two methods and compare the significant trends

# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)

# calculate trends with two different methods
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=0)
plot(AATmap)
STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=0)
plot(STMmap)

# classify the trend estimates from the two methods into significant positive, negative and no trend
AATmap.cl <- TrendClassification(AATmap)
plot(AATmap.cl, col=brgr.colors(3))
STMmap.cl <- TrendClassification(STMmap)
plot(STMmap.cl, col=brgr.colors(3))

# compare the two classifications
compare <- CompareClassification(x=AATmap.cl, y=STMmap.cl, names=list('AAT'=c("Br", "No", "Gr"), 'STM'=c("Br", "No", "Gr")))
compare

# plot the comparison
plot(compare)

})