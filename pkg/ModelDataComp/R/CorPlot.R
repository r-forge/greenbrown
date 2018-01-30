CorPlot <- structure(function(
	##title<< 
	## Correlation Matrix Plot
	##description<< 
	## Plots a matrix of variables with #1 scatterplots in the lower triangle, #2 correlation values (or other \code{\link{ObjFct}} metrics) in the upper triangle, and #3 histograms along the diagonal.
	
	x, 
	### data.frame or matrix with values that should be plotted
	
	objfct="Spearman",
	### Which objective function metric should be shown in the upper triangle of the matrix?
	
	cols = NULL,
	### vector of colours that should be used
		
	... 
	### further arguments to plot functions
	
	##details<< 
	## This function plots a matrix with scatter plots.
	
	##seealso<< 
	## \code{\link{ObjFct}}
) {

   # function to plot correlation
   panel.cor2 <- function(x, y, digits=2, prefix="", cols=NULL, objfct="Cor", of.all=NA, ...) {
	   # function to plot correlation coefficient with coloured background
	   require(fields)
      usr <- par("usr")
	   on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
	
	   xy <- data.frame(x,y)
	   xy <- na.omit(xy)
	   if (nrow(xy) < 2) {
		   xy <- data.frame(x=c(-1, -1), y=c(-1, -1))
	   }
	   xlim <- quantile(xy$x, c(0.01, 0.99))
	   ylim <- quantile(xy$y, c(0.01, 0.99))
	   sel <- xy$x >= xlim[1] & xy$x <= xlim[2] & xy$y >= ylim[1] & xy$y <= ylim[2]
	   xy <- xy[sel, ]
	   x <- xy$x
	   y <- xy$y
	   
	   # compute objective function
	   obj <- ObjFct(x, y)
	   of <- obj[[objfct]]
	   pval <- obj[[paste0(objfct, ".pval")]]
	
	   # format objective function
	   txt <- signif(of, 2)
	   txt <- paste(prefix, txt, sep="")
	   
	   # get pvalue
	   if (is.null(pval)) {
	      signif <- p <- ""
	   } else {
	      p <- format(c(pval, 0.123456789), digits=digits)[1]
	      signif <- symnum(pval, corr = FALSE, na = FALSE,
		      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
		      symbols = c("***", "**", "*", ".", " "))
	   }
	   
	   # make colors
	   cols <- ObjFctColours(c(of, of.all), objfct, cols=cols)

      # plot elements
	   rect(0, 0, 1, 1, col=cols$cols[1])
	   text(0.5, 0.5, txt, cex=(par()$cex*3))	
	   text(0.5, 0.8, signif, cex=(par()$cex*2.5), col="darkgrey")
   }

   # function to plot histogram
   panel.hist2 <- function(x, ...) {
	   # function to plot histogram
       usr <- par("usr")
	   on.exit(par(usr))
	   if (!all(is.na(x))) {
		   par(usr = c(usr[1:2], 0, 1.5) )
		   h <- hist(x, plot = FALSE)
		   breaks <- h$breaks
		   nB <- length(breaks)
		   y <- h$counts
		   y <- y/max(y)
		   rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
		   box()
	   } else {
		   rect(0, 0, 1, 1, ...)
	   }
   }

   # function to plot scatter and smotthing line
   panel.scatter <- function(x, y, pch = par("pch"), 
       col.smooth="red", span = 2/3, iter = 3, ...) {
	   # function to plot scatterplot with fitted smoothing spline
	   xy <- data.frame(x,y)
	   xy <- na.omit(xy)
	   if (nrow(xy) < 2) {
		   xy <- data.frame(c(-1, -1), c(-1, -1))
	   }
	   xlim <- quantile(xy$x, c(0.01, 0.99))
	   ylim <- quantile(xy$y, c(0.01, 0.99))
	   sel <- xy$x >= xlim[1] & xy$x <= xlim[2] & xy$y >= ylim[1] & xy$y <= ylim[2]
	   xy <- xy[sel, ]
	   x <- xy$x
	   y <- xy$y
		
	   d <- data.frame(cex=c(0.1, 0.8), n=c(1000, 100))
	   m <- lm(cex ~ n, d)
	   cex <- predict(m, data.frame(n=nrow(xy)))
	   cex[cex > 0.8] <- 0.8
	   cex[cex < 0.1] <- 0.1
	
	   if (nrow(xy) <= 1000) {	
		   points(x, y, pch = pch, cex=cex)
	   } else {
		   if (xlim[1] != xlim[2] & ylim[1] != ylim[2]) {
			   r <- raster(nrows=100, ncols=100, xmn=xlim[1], xmx=xlim[2], ymn=ylim[1], ymx=ylim[2])
			   r <- rasterize(as.matrix(xy), r, fun='count')
			   brks <- quantile(values(r), c(0, 0.001, seq(0.1, 1, 0.05)), na.rm=TRUE)
			   brks <- unique(brks)
			   cols <- rev(gray.colors(length(brks)+10))
			   cols <- cols[-(1:11)]
			   image(r, add=TRUE, breaks=brks, col=cols)
		   } else {
			   points(x, y, pch = pch, cex=cex)
		   }
	   }
	   ok <- is.finite(x) & is.finite(y)
	   if (any(ok)) {
		   abline(h=0, col="grey")
		   abline(v=0, col="grey")
		   lines(stats::lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
		   #try(lines(smooth.spline(x, y, spar=0.99), col=col.smooth, ...), silent=TRUE)
		   box()
	   }
   }

   # remove variables that are missing or are the same
   x <- apply(x, 2, function(x) as.numeric(x))
	keep <- apply(x, 2, function(x) !all(is.na(x)))
	x <- x[, keep]
	
	# compute objective functions
	com <- expand.grid(1:ncol(x), 1:ncol(x))
	of.all <- NA
	for (i in 1:nrow(com)) {
	   obj <- ObjFct(x[, com[i,1]], x[, com[i,2]])
	   of.all[i] <- obj[[objfct]]
	}
	
	# make plot
	if (is.null(dim(x))) {
		plot.new()
	} else {
		suppressWarnings(pairs(x, lower.panel=panel.scatter, upper.panel=panel.cor2, diag.panel=panel.hist2, gap=0, objfct=objfct, of.all=of.all, ...))
	}
}, ex=function() {

# create some data
a <- 1:30
b <- 30:1
c <- rnorm(30)
d <- a + rnorm(30, 0, 5)
e <- b + rnorm(30, 0, 15)
f <- a * exp(c)
x <- cbind(a, b, d, e, f)

# default plot with Spearman correlation 
CorPlot(x)

# other objective function metrics
CorPlot(x, objfct="Cor")
CorPlot(x, objfct="KS")
CorPlot(x, objfct="IoA")
CorPlot(x, objfct="MEF")
CorPlot(x, objfct="RMSE")

# other colours
CorPlot(x, objfct="RMSE", cols=c("blue", "red"))

# with text
CorPlot(x, objfct="Cor", cols=c("blue", "red"), prefix="r = ")

})





