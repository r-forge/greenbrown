AccuracyAssessment <- structure(function(
	##title<< 
	## Accuracy assessment from a contingency table
	
	##description<<
	## This function takes a contingency table as calculated with \code{\link{table}} or \code{\link[raster]{crosstab}} and computes an accuracy assessment, including the total accuracy, the user accuracy and the producer accuracy.
	
	tab
	### contingency table as calculated with \code{\link{table}} or \code{\link{crosstab}}
	
	##references<< Congalton, R.G. (1991): A review of assessing the accuracy of classifications of remotely sensed data. - Remote Sensing of Environment 1991, 37, 35-46.

	##seealso<<
	## \code{\link{CompareClassification}}, \code{\link{Kappa}}, \code{\link{TrendClassification}}	
) {

	r <- nrow(tab)
	c <- ncol(tab)

	# total accuracy
	totalacc <- sum(diag(tab)) / sum(tab) * 100
		
	# user accuracy
	useracc <- NA
	for (i in 1:r) {
		useracc[i] <- tab[i,i] / sum(tab[i,]) * 100
	}
	names(useracc) <- dimnames(tab)[[1]]
	
	# producer accuray
	produceracc <- NA
	for (i in 1:c) {
		produceracc[i] <- tab[i,i] / sum(tab[,i]) * 100
	}	
	names(produceracc) <- dimnames(tab)[[2]]
	
	# add results to table
	tab <- addmargins(tab)
	tab <- cbind(tab, UserAccuracy=c(useracc, NA))
	tab <- rbind(tab, ProducerAccuracy=c(produceracc, NA, NA))
	tab[r+2, c+2] <- totalacc
	
	return(tab)
	### The function returns the same frequency table as the input but with added row and column totals and total accuracy, user accuracy and producer accuracy.
}, ex=function() {

# two classifications: 
a <- c(1, 1, 1, 2, 2, 2, 3, 4, 5, 5, 5, 1, 1, 1, 2, 2, 2, 3, 4, 5, 5, 3, 3, 2, 2)
b <- c(1, 2, 1, 2, 2, 2, 3, 4, 2, 2, 5, 1, 2, 2, 2, 1, 2, 3, 4, 5, 5, 3, 3, 2, 2)

# calculate first a contingency table
tab <- table(a, b)

# calculate now the accuracy assessment
AccuracyAssessment(tab)

# calculate Kappa coeffcient
Kappa(tab)

})

