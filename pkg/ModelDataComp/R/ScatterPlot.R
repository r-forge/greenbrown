ScatterPlot <- structure(function(
	##title<< 
	## Enhanced scatterplot with fitting lines
	##description<< 
	## This function creates a scatterplot with fitting lines. Points and fitting lines can be also computed by groups.
	
	x, 
	### vector of x values
	
	y, 
	### vector of y values
	
	groups=NULL, 
	### vector of grouping variables (optional), in case of NULL all points are treated as one group

	plot.type=NULL, 
	### plot type: possible arguments are 'density' to plot density lines, 'points' to plot points, or 'image' to plot point counts as a raster image. In case of NULL, the optimal plot.type will be determined dependent on the number of values. It is also possible to combine plot types (e.g. c("image", "density")).
	
	objfct=FALSE,
	### Compute objective functions and add a 1:1 line to the plot?
	
	which.objfct=c("Cor", "Pbias", "RMSE"),
	### Which objective functions should be added to legend of the plot if objfct=TRUE?
	
	add=FALSE,
	### add plot to existing plot?
	
	density.levels=50, 
	### number of levels for density lines	
	
	density.labels=FALSE, 
	### add labels to density lines?	
	
	image.nbrks = 300,
	### minimum number of breaks for the image
	
	fits=c("spline"),
	### Fitting methods that should be used. See \code{\link{MultiFit}} for details. If several methods area provided the mean of all methods is plotted.
	
	fit.quantile = NULL,
	### Compute fit to a certain quantile? If NULL, fits to the mean. Otherwise sa value between 0 and 1 can be specified to fit to a certain quantile.

	fit.minnobs=10,
	### minimum number of observations per group in order to calculate a fitting curve
		
	fit.groups=TRUE, 
	### Plot fit lines for groups?
	
	fit.global=TRUE, 
	### Plot global fit lines?
		
	col.points="black", 
	### color for the points/groups
	
	col.image=NULL,
	### color range for image
	
	col.density=NULL,
	### color range for density lines
	
	col.fit=col.points,
	### colors for fitting lines
	
	col.global=NULL,
	### colors for global fittiing line

	xlab="", 
	### a title for the x axis
	
	ylab="", 
	### a title for the y axis
	
	main="", 
	### an overall title for the plot
	
	xlim=NULL, 
	### range for the x axis
	
	ylim=NULL, 
	### range for the y axis
	
	lwd = 2,
	### line width
	
	lty = 1,
	### line type
	
	quantile.x=c(0.01, 0.99),
	### lower and upper quantile to exclude extreme x values from plotting and fit calculations
	
	quantile.y=c(0.01, 0.99),
	### lower and upper quantile to exclude extreme y values from plotting and fit calculations
	
	plot=TRUE,
	### should a scatterplot be plotted or calculate only fitting lines
	
	cex=NULL, 
	### size of the points in the plot. If NULL point size will be calculated based on the number of points.
	
	pch=21,
	### point symbol
	
	alpha=NULL, 
	### transparency of the points (0...255)
	
	... 
	### further arguments to \code{\link{MultiFit}} or \code{\link{plot.default}}
	
	##details<< 
	## This function plots a scatterplot.
	
	##seealso<< 
	## \code{\link{ObjFct}}
) {

	# get information about groups
	has.groups <- TRUE
	ngroups <- 1
	if (is.null(groups)) {
		has.groups <- FALSE
	} else {
		has.groups <- TRUE
		if (all(is.na(groups))) has.groups <- FALSE
	}
	if (has.groups) {
		groups.unique <- unique(na.omit(groups))
		groups.unique <- groups.unique[order(groups.unique)]
		ngroups <- length(groups.unique)
		if (length(col.points) != ngroups) {
			col.points <- c(piratepal("xmen"), piratepal("pony"))[1:ngroups]
		}
		groups.id <- match(groups, groups.unique)
	} else {
		groups.id <- rep(1, length(x))
		groups <- groups.id
	}
	
	# check how the plot should look like
	if (is.null(plot.type)) {
		if (has.groups) {
			plot.type <- "points"
		} else {
			if (length(x) > 3000) {
				plot.type <- "image"
			} else {
				plot.type <- "points"
			}
		}
	}
		
	# prepare data
	data.df <- data.frame(x, y, groups, pch)
	data.df <- na.omit(data.df)
	x <- data.df$x
	y <- data.df$y
	pch <- data.df$pch
	groups <- data.df$groups
	
	# calculate limits of x and y axis
	if (is.null(xlim) & objfct) {
	   xlim <- ylim <- quantile(c(x, y), prob=quantile.x, na.rm=TRUE)
	   ylim[2] <- ylim[2] + diff(ylim) * 0.1
	}
	if (is.null(ylim)) ylim <- quantile(y, prob=quantile.y, na.rm=TRUE)
	if (is.null(xlim)) xlim <- quantile(x, prob=quantile.x, na.rm=TRUE)	
	
	# calculate density lines
	if (any(grepl("density", plot.type))) {
		bdw <- c(bandwidth.nrd(x), bandwidth.nrd(y))
		bdw[bdw <= 0] <- 0.01
		yx.density <- kde2d(x, y, h=bdw)
		if (is.null(col.density)) col.density <- grey(seq(1, 0.2, by=-0.1))

		# can plot density?
		if (all(is.na(yx.density$z))) {
			plot.type <- "points"
			warning("Cannot compute kernel density.")
		} 
	}

	# convert points to image
	if (any(grepl("image", plot.type))) {
	   x.brks <- pretty(x, n=(image.nbrks+5), min.n=image.nbrks)
	   y.brks <- pretty(y, n=(image.nbrks+5), min.n=image.nbrks)
	   ext <- extent(min(x.brks), max(x.brks), min(y.brks), max(y.brks))
	   r <- raster(ext, ncols=(length(x.brks)-1), nrows=(length(y.brks)-1))
	   pts.fs <- rasterize(cbind(x, y), r, fun='count')
	   pts.fs[] <- log(values(pts.fs))
	   if (is.null(col.image)) col.image <- grey(seq(1, 0.2, by=-0.1))
	   col.fun <- colorRampPalette(col.image)
		col.img <- col.fun(50)
	}

   # calculate point size and half transparency
   col.orig <- col.points
	if (any(grepl("points", plot.type))) {
		if (is.null(cex)) {
			d <- data.frame(cex=c(0.1, 1), n=c(5000, 300))
			m <- lm(cex ~ n, d)
			cex <- predict(m, data.frame(n=nrow(data.df)))
			cex[cex > 0.9] <- 0.9
			cex[cex < 0.3] <- 0.3
			cex[nrow(data.df ) < 300] <- 1
		}
		if (is.null(alpha)) {
			alpha <- cex * 200
			alpha[alpha < 60] <- 60
			alpha[alpha > 200] <- 200
			alpha[nrow(data.df) < 300 & !has.groups] <- 255
			alpha[nrow(data.df) < 300 & has.groups] <- 180
		}	
		col.points <- col2rgb(col.points)
		col.points <- rgb(col.points[1,], col.points[2,], col.points[3,], alpha, maxColorValue=255)
	}

	# make plot
	if (plot) {
		if (!add) plot(x, y, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=main, ...)
		if (any(grepl("image", plot.type))) {
		   image(pts.fs, add=TRUE, col=col.img)
			box()
		}
		if (any(grepl("points", plot.type))) {
			points(x, y, col=col.points[groups.id], bg=col.points[groups.id], cex=cex, pch=pch)
		}
		if (any(grepl("density", plot.type))) {
         contour(yx.density, nlevels=density.levels, drawlabels=density.labels, col=col.density, add=TRUE)
      }
		if (objfct) abline(0, 1)
	}
		
	# add fit lines for groups
	results <- list(NULL)
	if (fit.groups & has.groups) {
		fit.l <- list(NULL)
		for (k in 1:ngroups) {
			gr <- groups.unique[k]
			xs <- data.df$x[(data.df$groups == gr)]
			ys <- data.df$y[(data.df$groups == gr)]
			if ((length(xs) >= fit.minnobs) & (length(ys) >= fit.minnobs) & !AllEqual(na.omit(xs)) & !AllEqual(na.omit(ys))) {
				fit <- MultiFit(xs, ys, fits=fits, excl.quantile=quantile.x, fit.quantile=fit.quantile)
				if (plot) lines(fit$x, fit$ensMean, col=col.orig[k], lwd=lwd, lty=lty)
				fit.l[[k]] <- fit
			} else {
				fit.l[[k]] <- NA
			}
		}
		results$groups.fit <- fit.l
	} # end fit line
	
	# add global fit
	if (fit.global & !AllEqual(data.df$x) & !AllEqual(data.df$y)) {
		fit <- MultiFit(x=data.df$x, y=data.df$y, fits=fits, excl.quantile=quantile.x, fit.quantile=fit.quantile)
		if (is.null(col.global)) { 
			col.global <- "red"
			if (has.groups) col.global <- "black"
		}
		if (plot) lines(fit$x, fit$ensMean, col=col.global, lwd=lwd, lty=lty)	
		results$global.fit <- fit
	}
	obj <- ObjFct(y, x, groups)
	if (plot & objfct) {
	   pos <- "bottomleft"
	   medr <- median(obj$Cor)
	   medr[is.na(medr)] <- 0
		if (medr > 0) pos <- "topleft"
		if (has.groups) legend(pos, ObjFct2Text(obj, which=which.objfct, sep=" "), bty="n", text.col=c(col.global, col.orig))
		if (!has.groups) legend(pos, ObjFct2Text(obj, which=which.objfct, sep=" ")[1], bty="n", text.col=c(col.global, col.orig))
	}
	if (objfct) results$ObjFct <- obj

	return(results)
	### The function returns a list with the computed fitting lines and objective functions per group.
}, ex=function() {

# create some data:
n <- 10000
x <- runif(n, 0, 100)
groups <- c(rep(1, 3000), rep(2, 3000), rep(3, 4000))
y <- (3 * x^(1/2) + rnorm(n, 0, x/20)) * groups

# standard plot: not very well distinguishable
plot(x, y)

# ScatterPlot 
result <- ScatterPlot(x, y)

# ScatterPlot with coulored groups and fitting lines
result <- ScatterPlot(x, y, groups)

# different plot types 
result <- ScatterPlot(x, y, plot.type="points")
result <- ScatterPlot(x, y, plot.type="density")
result <- ScatterPlot(x, y, plot.type=c("image", "density"))

# plot and compute objective functions
result <- ScatterPlot(x, y, groups, objfct=TRUE)
result

# plot fits to upper 0.9 and lower 0.1 quantiles, mean fit from two methods
result <- ScatterPlot(x, y, groups, fits=c("poly3", "spline"), 
 fit.quantile=0.9, plot.type="image", fit.global=FALSE)
result <- ScatterPlot(x, y, groups, fits=c("poly3", "spline"), 
 fit.quantile=0.1, plot.type=NA, add=TRUE, fit.global=FALSE)


})





