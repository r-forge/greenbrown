PolygonNA <- structure(function(
	##title<< 
	## Plot a polygon by accounting for NA values (breaks in polygon)
	
	##description<<
	## This function is an improved version of \code{\link{polygon}} that considers NA values in plotting.
	
	x, 
	### vector of x-values
	
	lower, 
	### vector of lower polygon range
	
	upper, 
	### vector of upper polygon range
	
	col="grey"
	### color of the polygon
	
) {

	df <- data.frame(x, lower, upper)
	df <- df[!is.na(df$x), ]
	x <- df$x
	lower <- df$lower
	upper <- df$upper
	
	# fill lower with median value if only lower is NA
	bool <- is.na(lower) & !is.na(upper)
	if (any(bool)) {
		lower[bool] <- median(lower, na.rm=TRUE)
	}
	
	# fill upper with median value if only upper is NA
	bool <- !is.na(lower) & is.na(upper)
	if (any(bool)) {
		upper[bool] <- median(upper, na.rm=TRUE)
	}	

	# check if both lower and upper values are NA
	bool <- is.na(lower) & is.na(upper)
	if (any(bool)) {
		# identify blocks of non-NA values
		blocks <- NULL
		block <- 1	
		for (i in 1:length(x)) {
			if (!bool[i]) blocks[i] <- block
			if (bool[i]) {
				blocks[i] <- 0
				block <- block + 1
			}
		}
							
		# plot polygon for each block
		for (i in unique(na.omit(blocks))) {
			if (i > 0) {
				sel <- i == blocks
				polygon(x=c(x[sel], rev(x[sel])), y=c(lower[sel], rev(upper[sel])), col=col, border=NA)
			}
		}
	} else {
		polygon(x=c(x, rev(x)), y=c(lower, rev(upper)), col=col, border=NA)
	}
}, ex=function(){

x <- 1:10
med <- rnorm(length(x))
lower <- med - 2
upper <- med + 2

# example 1: no NA values
plot(x, med, type="l", ylim=range(c(lower, upper), na.rm=TRUE))
PolygonNA(x, lower, upper)
lines(x, med)

# example 2: with some NA values 
lower1 <- lower
upper1 <- upper
lower1[c(1, 6, 10)] <- NA
upper1[c(1:2, 6)] <- NA
plot(x, med, type="l", ylim=range(c(lower, upper), na.rm=TRUE))
PolygonNA(x, lower1, upper1)
lines(x, med)


})

