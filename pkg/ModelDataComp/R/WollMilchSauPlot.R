WollMilchSauPlot <- structure(function(
	##title<< 
	## Compare means, distributions, and correlations of different datasets 
	##description<<
	## Plots a \code{\link{pirateplot}} with couulors according to model performance. The function can be used to compare means, distributions, and correlations (or any other metric from \code{\link{ObjFct}}) between different datasets and models. Thus this plot is almost an "eier-legende Wollmichsau" (german, animal that produces eggs, wool, milk and meet) for model-data comparison. The plot is based on the function \code{\link{pirateplot}}
	
	x,
	### a data.frame with at least two columns
	
	ref = 1,
	### Which column in x is the reference dataset? Also more than one reference can be provided, e.g. ref = c(1,2) will compute the objfct based on the combination of both datasets. 
	
	objfct = "Cor",
	### Which objective function metric should be used to create the colour palette? (see \code{\link{ObjFct}})
	
	cols = tim.colors(10),
	### vector of colors from which the color palette should be interpolated
	
	brks = NULL,
	### break for colour scale
	
	names = NULL,
	### names of the datasets
	
	main = NULL,
	### title of the plot
	
	xlab = NULL,
	### label for x-axis
	
	ylab = NULL,
	### label for y-axis
	
	xlim = NULL,
	### limits for x-axis
	
	ylim = NULL,
	### limits for y-axis
	
	legend = TRUE,
	### plot a legend?
	
	legend.only = FALSE,
	### plot only a legend?
	
	point.pch = NA,
	### point types, default: no points
	
	bar.o = 1, 
	### opacity of bars
	
	bean.o = 1, 
	### opacity of beans
	
	inf.o = 0,
	### inference bands
	
	bean.lwd = 1, 
	### line width of beans
	
	line.lwd = 1, 
	### line width of lines
	
	cut.min = NULL,
	### Optional minimum value of the beans.
	
	cut.max = NULL,
	### Optional maximum value of the beans.
	
	...
	### further arguments to \code{\link{pirateplot}}
	
	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{pirateplot}}, \code{\link{ObjFct}}, \code{\link{TaylorPlot}}, \code{\link{WollMilchSauPlot}}, \code{\link{ScatterPlot}}
) {
   op <- par()
   x <- na.omit(x)
   ndatasets <- ncol(x)
   
   if (is.null(xlab)) xlab <- ""
   if (is.null(ylab)) ylab <- "y"
   if (is.null(main)) main <- ""
   if (is.null(names)) {
      names <- c(colnames(x)[ref], colnames(x)[(1:ncol(x))[-ref]])
   }
   
   if (is.null(cut.min) & all(x >= 0)) cut.min <- 0
   if (is.null(cut.max) & all(x <= 0)) cut.max <- 0

   # restructure data
   x2.df <- data.frame(y=x[,ref[1]], x=colnames(x)[ref[1]])
   for (i in (1:ncol(x))[-ref[1]]) x2.df <- rbind(x2.df, data.frame(y=x[,i], x=colnames(x)[i]))
   
   if (!is.null(objfct)) {
      # compute performance metrics
      objfct.l <- llply(as.list(1:ncol(x)), function(n) {
         if (length(ref) == 1) {
            ObjFct(x[,n], x[, ref]) 
         } else {
            r <- ref[n != ref] # compute only against other ref
            sim <- rep(x[,n], length(r))
            obs <- unlist(x[,r])
            ObjFct(sim, obs) 
         }
      })
      
      # extract to be used for colour
      of.col <- unlist(llply(objfct.l, function(of) unlist(of[grep(objfct, names(of))[1]])))

      # optimum objective function
      of.best <- ObjFct(1:10, 1:10)
      of.best <- unlist(of.best[grep(objfct, names(of.best))[1]])
      
      # create breaks for color scale
      of <- c(of.col, of.best)
      if (objfct == "Cor" | objfct == "Spearman") of <- c(of, -1)
      if (objfct == "R2" | objfct == "IoA") of <- c(of, 0)
      if (objfct == "KS") of <- c(of, 1)
      if (objfct == "FV") of <- c(of, -2, 2)
      if (is.null(brks)) brks <- pretty(of, min.n=10, n=15)
      
      # create colors
      cols0 <- colorRampPalette(cols)
      cols0 <- cols0(length(brks)-1)
      if (of.best > 0) cols0 <- rev(cols0)
      cols <- cols0[cut(of.col, brks)]
      if (length(ref) == 1) cols[ref] <- "black" 
      
#      # apply transparency to legend
#      if (bar.o < 1) {
#         cols0 <- col2rgb(cols0)
#         cols0 <- rgb(cols0[1,], cols0[2,], cols0[3,], 255*bar.o, maxColorValue=255)
#      }
   } else {
      if (length(cols) != ndatasets) cols <- piratepal("basel", length.out=ndatasets)
      legend <- FALSE
   }
   
   # plot only legend?
   if (legend & legend.only) {
      plot.new()
      image.plot(zlim=brks, legend.only=TRUE, col=cols0, breaks=brks, legend.args=list(text=objfct, cex=1.2, font=2, line=0.5))
      return()
   }
  
   # y axis limits
   if (is.null(ylim)) {
      ylim <- aggregate(y ~ x, data=x2.df, quantile, prob=c(0.025, 0.975), na.rm=TRUE)
      o1 <- order(ylim$y[,1], decreasing=FALSE)
      o2 <- order(ylim$y[,2], decreasing=TRUE)
      ylim <- c(ylim$y[,1][o1[2]], ylim$y[,2][o2[2]]) # take second highest quantile 0.975
      m <- aggregate(y ~ x, data=x2.df, mean, na.rm=TRUE)$y
      if (any(m < ylim[1])) ylim[1] <- min(m) - diff(ylim) * 0.03
      if (any(m > ylim[2])) ylim[2] <- max(m) + diff(ylim) * 0.03
   }
   
   # x-axis limits
   if (is.null(xlim)) {
      if (legend) xlim <- c(0.5, ndatasets+1.8)
      if (!legend) xlim <- c(0.5, ndatasets+0.5)
   }
   
   # make plot
   cex <- 1.3
   par(mfrow=c(1,1), mar=c(3.7, 3.5, 2.5, 1.5), oma=c(0.8, 0.1, 0.1, 0.1), mgp=c(2.4, 1, 0), cex=cex, cex.lab=cex*1.1, cex.axis=cex*1.1, cex.main=cex*1.1, xpd=FALSE)
   pirateplot(y ~ x, data=x2.df, ylim=ylim, xlim=xlim, cut.min=cut.min, xlab=xlab, ylab=ylab, yaxt="n", xaxt="n", bean.border.col=cols, average.line.col=cols, pal=cols, at=1:ndatasets, point.pch = point.pch, bar.o = bar.o, bean.o = bean.o, inf.o = inf.o, bean.lwd = bean.lwd, line.lwd = line.lwd, ...)
   
   # add axes
   yaxt <- pretty(seq(ylim[1], ylim[2], length=15))
   axis(2, yaxt, yaxt)
   xaxt <- 1:ndatasets
   axis(1, xaxt, rep("", ndatasets))
   par(xpd=TRUE)
   ypos <- ylim[1] - diff(ylim) * 0.1
   text(1:ndatasets, rep(ypos, ndatasets), names, srt=16)
   box()
   mtext(main, 3, 0.5, font=2, cex=1.5)
   par(xpd=FALSE)
   
   # add legend bar
   if (legend) {
      m <- lm(f ~ l, data.frame(l=xlim, f=c(0, 1)))
      p1 <- predict(m, data.frame(l=ndatasets+0.5))
      p2 <- predict(m, data.frame(l=ndatasets+0.7))
      image.plot(zlim=brks, legend.only=TRUE, col=cols0, breaks=brks, smallplot=c(p1, p2, 0.2, 0.8), legend.args=list(text=objfct, cex=1.2, font=2, line=0.5))
   }
   
}, ex=function() {

# create some data
obs <- rlnorm(500, 1, 1) # observations
sim1 <- obs * rnorm(500, 1, 0.5) # similar to obs
sim2 <- obs * rnorm(500, 1, 2) # less similar to obs
sim3 <- obs * rnorm(500, 1, 4) # less similar to obs
sim4 <- rlnorm(500, 1, 1) # same distribution but no correlation
sim5 <- rnorm(500, 4.4, 2) # similar mean but different distribution
x <- data.frame(obs, sim1, sim2, sim3, sim4, sim5)
x[x < 0] <- 0

# default plot
WollMilchSauPlot(x)

# with different objective function as colour
WollMilchSauPlot(x, objfct="IoA")
WollMilchSauPlot(x, objfct="Pbias")
WollMilchSauPlot(x, objfct="FV")

# some other settings, e.g. with plotting points
WollMilchSauPlot(x, ylab="Area (km2)", xlab="Groups", main="Comparison", 
 point.pch=16, bar.o=0.5, point.o=0.4)

# different color palettes
WollMilchSauPlot(x, cols=c("blue", "red"))
WollMilchSauPlot(x, cols=rainbow(10))
WollMilchSauPlot(x, cols=heat.colors(5))

# without legend (but using an objective function to colour)
WollMilchSauPlot(x, legend=FALSE)

# only legend
WollMilchSauPlot(x, legend.only=TRUE)

# without using an objective function 
WollMilchSauPlot(x, objfct=NULL)

# different example data
obs <- rnorm(500, 5, 1)
sim1 <- obs * rnorm(500, 1, 0.2) # similar to obs
sim2 <- obs * rnorm(500, 2, 1) # bias
sim3 <- obs * rlnorm(500, 1, 0.1) # less similar to obs but highly correlated
x <- data.frame(obs, sim1, sim2, sim3)
WollMilchSauPlot(x)


})




