SelectQuantiles <- structure(function(
	##title<< 
	## Select values around quantiles
	##description<<
	## The function selects  from a set of points along the range of x values these points that are close to a certain quantile of y. First, x values are classified in n groups. Secondly, the specified quantile of y is computed for each group. Thirdly, all y values in a group that are outside +- 0.05 of the estimated quantile are removed. See examples for an illustration.
	
	x, 
	### vector of x values
	
	y, 
	### vector of y values
	
	q=0.5,
	### quantile value for which data should be selected
	
	n = NULL,
	### number of classes for x
	
	...
	### further arguments
	
	##details<<
	## 	
	##seealso<<
	## 
		
	##values<<
	## A list with the components x (fitted x values), y (fitted y values) and fit (object of \code{\link{lm}}, \code{\link{smooth.spline}} or \code{\link{rq}}. 
	
) {
   d <- na.omit(data.frame(x, y))
   d$x <- as.numeric(d$x)
   
   # split x in classes
	if (is.null(n)) n <- nclass.Sturges(d$x)
	cl <- classInt::classIntervals(d$x, n=n, style="quantile")
	d$xcl <- findInterval(d$x, cl$brks)
		
	# add classes with low number of values to previous class
	cltab <- table(d$xcl)
	for (c in 1:length(unique(d$xcl))) {
		if (cltab[c] < 20) {
			if (c == 1) d$xcl[d$xcl == c] <- c + 1
			if (c > 1) d$xcl[d$xcl == c] <- c - 1
		}
	}
	d <- d[order(d$xcl), ]
	# plot(d$x, d$y, col=d$xcl)
			
	# exclude for each class values +-5% of the desired quantile 
	cl.quant <- by(d$y, list(d$xcl), FUN=function(x) {
		quantile.up <- q + 0.05
		quantile.low <- q - 0.05
		if (quantile.up > 1) {
			diff <- 0.05 - (quantile.up - 1)
			quantile.up <- q + diff
			quantile.low <- q - diff
		}
		if (quantile.low < 0) {
			diff <- 0.05 - (0 - quantile.low)
			quantile.up <- q + diff
			quantile.low <- q - diff
		}
		quantile.low[quantile.low < 0] <- 0
		quantile.up[quantile.up > 1] <- 1
		qup <- quantile(x, quantile.up) 
		qlow <- quantile(x, quantile.low) 
		x[x > qup] <- NA
		x[x < qlow] <- NA
		return(x)
	})
		
	# fit a spline through the remaining data
	dnew <- data.frame(x=d$x, y=unlist(cl.quant))
	dnew <- na.omit(dnew)
	return(dnew)
}, ex=function() {

# x and y points
x <- 1:1000
y <- x * rnorm(1000, 1, 3)
plot(x, y)

# select points that are close to the median
q05 <- SelectQuantiles(x, y)
points(q05$x, q05$y, col="red")

# select points that are close to the 0.9 quantile
q09 <- SelectQuantiles(x, y, 0.9)
points(q09$x, q09$y, col="blue")

# select points that are close to the 0.1 quantile
q01 <- SelectQuantiles(x, y, 0.1)
points(q01$x, q01$y, col="purple")

# the selected points can be used for fits to a specific quantile
abline(lm(y ~ x, q09), col="blue")
abline(lm(y ~ x, q01), col="purple")


})


	
