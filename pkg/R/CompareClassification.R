CompareClassification <- structure(function(
	##title<< 
	## Compare two raster layers with classified values
	
	##description<<
	## This function computes an agreement map of two classifications (RasterLayers with classified values). Additionally, it computes a frequency table with user, producer and total accuracies as well as the Kappa coefficient. 
	
	x,
	### First raster layer with classification.
	
	y,
	### Second raster layer with classification.
	
	names=NULL
	### a list with names of the two classifications and class names. See example section for details.
		
	##seealso<<
	## \code{\link{plot.CompareClassification}}, \code{\link{AccuracyAssessment}}, \code{\link{TrendClassification}}

) {
	# crop x and y to same extent
	same.ext <- compareRaster(x, y, extent=TRUE, rowcol=TRUE, crs=FALSE, res=TRUE, orig=TRUE, rotation=TRUE) 

	# get unique class IDs
	classes <- na.omit(unique(c(unique(values(x)), unique(values(y)))))
	classes <- classes[order(classes)]
	ncl <- length(classes)

	# class combinations
	class.comb <- expand.grid(classes, classes)
	ncomb <- nrow(class.comb)
	class.comb <- data.frame(id=1:ncomb, class.comb)

	# create new raster with class agreement
	r <- stack(x, y)
	agree.r <- calc(r, fun=function(xy) {
		x <- xy[1]
		y <- xy[2]
		if (is.na(x)) return(NA)
		if (is.na(y)) return(NA)
		agree <- class.comb$id[(x == class.comb[,2]) & (y == class.comb[,3])]
		return(agree)
	})
	
	# calculate contingency table
	tab <- crosstab(x, y)
	
	# names for the table
	if (is.null(names)) {
		names <- list(x=classes, y=classes)
	}	
	dimnames(tab) <- names
	
	# calculate accuracy assessment
	aa <- AccuracyAssessment(tab)
	
	result <- list(raster=agree.r, table=aa, kappa=Kappa(tab))
	class(result) <- "CompareClassification"
	return(result)
	### The function returns a list of class "CompareClassification" with the following components:
	### \itemize{ 
	### \item{ \code{raster} a raster layer indicating the agreement of the two classifications. }
	### \item{ \code{table} a contingency table with user, producer and total accuracies. Rows in the table correpond to the classification x, columns to the classifcation y. }
	### \item{ \code{kappa} Kappa coefficient. }
	### }
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
compare <- CompareClassification(AATmap.cl, STMmap.cl, names=list('AAT'=c("Br", "No", "Gr"), 'STM'=c("Br", "No", "Gr")))
compare

# plot the comparison
plot(compare)

})