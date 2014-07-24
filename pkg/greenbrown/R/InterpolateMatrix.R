InterpolateMatrix <- structure(function(
	##title<<
	## Interpolate NA values in a matrix using a moving mean
	
	##description<< This function interpolates missing values in a matrix with the mean of the neighbouring matrix cells.

	m
	### a matrix with NA value to interpolate

) {
	nrow <- nrow(m)
	ncol <- ncol(m)

	df <- data.frame(r=rep(1:nrow, ncol), c=rep(1:ncol, each=nrow), v=as.vector(m))
	reg <- lm(v ~ r*c, data=df)
	new <- predict(reg, df)
	new <- (new - min(new)) * ( (max(df$v, na.rm=TRUE) - min(df$v, na.rm=TRUE)) / (max(new) - min(new))) + min(df$v, na.rm=TRUE)
	m <- matrix(new, nrow=nrow, ncol=ncol)
	return(m)
	### matrix with interpolated values
}, ex=function() {
	m <- matrix(1:25, 5, 10)
	m[sample(1:50, 10)] <- NA
	m
	InterpolateMatrix(m)
})
