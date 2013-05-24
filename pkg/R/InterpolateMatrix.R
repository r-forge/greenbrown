InterpolateMatrix <- structure(function(
	##title<<
	## Interpolate NA values in a matrix using a moving mean
	
	##description<< This function interpolates missing values in a matrix with the mean of the neighbouring matrix cells.

	m,
	### a matrix

	iter=100
	### maximum number of iterations for the moving window
) {
	nrow <- nrow(m)
	ncol <- ncol(m)
	r <- raster(m)
	r2 <- focal(r, 3, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
	sum.na <- sum(is.na(matrix(values(r2), nrow=nrow, ncol=ncol)))
	r2[is.nan(r2)] <- NA
	i <- 1
	while (sum.na > 0 & i <= iter) {
		r2 <- focal(r2, 3, mean, na.rm=TRUE, pad=TRUE)
		sum.na <- sum(is.na( matrix(values(r2), nrow=nrow, ncol=ncol)))
		r2[is.nan(r2)] <- NA
		i <- i + 1
	}
	m3 <- matrix(values(r2), nrow=nrow, ncol=ncol)
	m[is.na(m)] <- m3[is.na(m)]
	return(m)
	### matrix with interpolated values
}, ex=function() {
	m <- matrix(1:25, 5, 10)
	m[sample(1:50, 10)] <- NA
	m
	InterpolateMatrix(m)
})
