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
	
	cols = NULL,
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
	
	cut.min = NULL,
	### Optional minimum value of the beans.
	
	cut.max = NULL,
	### Optional maximum value of the beans.
	
	avg = TRUE,
	### plot average line?
	
	points = TRUE,
	### plot points?
	
	bean = TRUE,
	### plot beans (density estimates)?
	
	inf = TRUE,
	### plot inference bands around mean?
	
	bar = TRUE,
	### plot bars?
	
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
      if (is.null(cols)) cols <- c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000")
      cols0 <- colorRampPalette(cols)
      cols0 <- cols0(length(brks)-1)
      if (of.best > 0) cols0 <- rev(cols0)
      cols <- cols0[cut(of.col, brks)]
      if (length(ref) == 1) cols[ref] <- "black" 
      
   } else {
      if (is.null(cols)) cols <- piratepal("basel", length.out=ndatasets)
      if (length(cols) != ndatasets) cols <- piratepal("basel", length.out=ndatasets)
      legend <- FALSE
   }
   
   # plot only legend?
   if (legend & legend.only) {
      plot.new()
      fields::image.plot(zlim=brks, legend.only=TRUE, col=cols0, breaks=brks, legend.args=list(text=objfct, cex=1.2, font=2, line=0.5))
      return()
   }
  
   # y axis limits
   if (is.null(ylim)) {
      pstats <- pirateplot(y ~ x, data=x2.df, plot=FALSE)
      m <- pstats$summary$avg
      m.lb <- pstats$summary$inf.lb
      m.ub <- pstats$summary$inf.ub
      if (bean | points) {
         ylim <- aggregate(y ~ x, data=x2.df, quantile, prob=c(0.025, 0.975), na.rm=TRUE)
         o1 <- order(ylim$y[,1], decreasing=FALSE)
         o2 <- order(ylim$y[,2], decreasing=TRUE)
         ylim <- c(ylim$y[,1][o1[2]], ylim$y[,2][o2[2]]) # take second highest quantile 0.975
      } else {
         if (avg) {
            ylim <- range(m)
         }
         if (inf) {
            ylim <- range(c(m.lb, m.ub))
         }
      }
      
      # check if average and inference bands are within limits
      if (avg) {
         if (any(m < ylim[1])) ylim[1] <- min(m) - diff(ylim) * 0.03
         if (any(m > ylim[2])) ylim[2] <- max(m) + diff(ylim) * 0.03
      }
      if (inf) {
         if (any(m.lb < ylim[1])) ylim[1] <- min(m.lb) - diff(ylim) * 0.03
         if (any(m.ub > ylim[2])) ylim[2] <- max(m.ub) + diff(ylim) * 0.03
      }
      if (!is.null(cut.min)) {
         if (ylim[1] < cut.min) ylim[1] <- cut.min
      }
      if (!is.null(cut.max)) {
         if (ylim[2] > cut.max) ylim[2] <- cut.max
      }
   }
   
   # x-axis limits
   if (is.null(xlim)) {
      if (legend) xlim <- c(0.5, ndatasets+1.8)
      if (!legend) xlim <- c(0.5, ndatasets+0.5)
   }
   
   # settings for plotting of elements
   avg.line.o <- 1
   if (!avg) avg.line.o <- 0
   bar.f.o <- 0.3
   if (!bar) bar.f.o <- 0
   inf.f.o <- 0.6
   if (!inf) inf.f.o <- 0
   bean.o <- 1
   if (!bean) bean.o <- 0
   point.pch <- 16
   if (!points) point.pch <- NA
   
   # make plot
   cex <- 1.3
   par(mfrow=c(1,1), mar=c(3.7, 3.5, 2.5, 1.5), oma=c(0.8, 0.1, 0.1, 0.1), mgp=c(2.4, 1, 0), cex=cex, cex.lab=cex*1.1, cex.axis=cex*1.1, cex.main=cex*1.1, xpd=FALSE)
   plot(1, 1, type="n", ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, yaxt="n", xaxt="n")
   pirateplot(y ~ x, data=x2.df, add=TRUE, at=1:ndatasets, yaxt="n", xaxt="n", 
      cut.min=cut.min, pal=cols, 
      point.pch = point.pch, # plot points?
      bean.b.o=bean.o, bean.b.col=cols, bean.f.o=bean.o, bean.f.col="white", # looking of beans
      inf.b.o=0, inf.b.col=cols, inf.f.o=inf.f.o, inf.f.col=cols, # looking of inference bands
      bar.b.o=0, bar.b.col=cols, bar.f.o=bar.f.o, bar.f.col=cols, # looking of bars
      avg.line.o=avg.line.o, avg.line.col=cols # looking of average line
      )

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
      fields::image.plot(zlim=brks, legend.only=TRUE, col=cols0, breaks=brks, smallplot=c(p1, p2, 0.2, 0.8), legend.args=list(text=objfct, cex=1.2, font=2, line=0.5))
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

# axis labels and title
WollMilchSauPlot(x, ylab="Area (km2)", xlab="Groups", main="Comparison")

# remove certain elements from plot
WollMilchSauPlot(x, points=FALSE)
WollMilchSauPlot(x, bean=FALSE)
WollMilchSauPlot(x, points=FALSE, bean=FALSE)
WollMilchSauPlot(x, points=FALSE, bean=FALSE, bar=FALSE)
WollMilchSauPlot(x, inf=FALSE)
WollMilchSauPlot(x, inf=FALSE, avg=FALSE)
WollMilchSauPlot(x, avg=FALSE, bar=FALSE, inf=FALSE)

# different color palettes
WollMilchSauPlot(x, cols=c("blue", "red"))
WollMilchSauPlot(x, cols=c("blue", "grey", "red"))
WollMilchSauPlot(x, cols=rainbow(10))
WollMilchSauPlot(x, cols=heat.colors(5))

# without legend (but using an objective function to colour)
WollMilchSauPlot(x, legend=FALSE)

# only legend
WollMilchSauPlot(x, legend.only=TRUE)

# without using an objective function - categorial colours
WollMilchSauPlot(x, objfct=NULL)

# different example data
obs <- rnorm(500, 5, 1)
sim1 <- obs * rnorm(500, 1, 0.2) # similar to obs
sim2 <- obs * rnorm(500, 2, 1) # bias
sim3 <- obs * rlnorm(500, 1, 0.1) # less similar to obs but highly correlated
x <- data.frame(obs, sim1, sim2, sim3)
WollMilchSauPlot(x)


})




