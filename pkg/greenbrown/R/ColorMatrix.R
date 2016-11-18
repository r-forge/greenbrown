ColorMatrix <- structure(function(
	##title<<
	## Create a square matrix of colors
	
	##description<< This function creates a square matrix with two diagonal crossing color ramps. It can be used to plot contingency maps of two classifications.

	dim=3, 
	### number of rows and number of columns of the matrix (only square matrix are possible, i.e. number of rows = number columns)
	
	ul="burlywood4", 
	### starting color in the upper left corner of the matrix
	
	lr="darkgreen", 
	### ending color in the lower right corner of the matrix	
	
	ll="khaki1", 
	### starting color in the lower left corner of the matrix
	
	ur="royalblue1", 
	### ending color in the upper right corner of the matrix	
	
	ctr="gray87"
	### color in the center of the matrix
	) {
	nrow <- ncol <- dim
	col.m <- matrix(NA, ncol=ncol, nrow=nrow)
	col.m[1,1] <- ul
	col.m[nrow, ncol] <- lr
	col.m[1, ncol] <- ur
	col.m[nrow, 1] <- ll
	col.m[median(1:nrow),median(1:ncol)] <- ctr
	col.rgb <- col2rgb(col.m)
	col.rgb <- apply(col.rgb, 2, function(x) {
		if ( all(x == 255) ) x <- rep(NA, 3)
		return(x)
	})
	red.m <- matrix(col.rgb[1,], ncol=ncol, nrow=nrow)
	red.m <- InterpolateMatrix(red.m)
	red.m[red.m < 0] <- 0
	red.m[red.m > 255] <- 255
	green.m <- matrix(col.rgb[2,], ncol=ncol, nrow=nrow)
	green.m <- InterpolateMatrix(green.m)	
	green.m[green.m < 0] <- 0
	green.m[green.m > 255] <- 255
	blue.m <- matrix(col.rgb[3,], ncol=ncol, nrow=nrow)
	blue.m <- InterpolateMatrix(blue.m)		
	blue.m[blue.m < 0] <- 0
	blue.m[blue.m > 255] <- 255
	col <- rgb(as.vector(red.m), as.vector(green.m), as.vector(blue.m), maxColorValue=255)
	col.m[is.na(col.m)] <- col[is.na(col.m)]
	return(col.m)
	### The function returns a square matrix of color names. 
}, ex=function() {
	col.m <- ColorMatrix()
	plot.new()
	legend("topleft", as.vector(col.m), fill=col.m, ncol=3)
	
	col.m <- ColorMatrix(dim=5, ul="red", ll="navy", ctr="purple")
	plot.new()
	legend("topleft", as.vector(col.m), fill=col.m, ncol=5)
})