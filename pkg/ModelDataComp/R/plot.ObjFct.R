plot.ObjFct <- structure(function(
	##title<< 
	## Plot objective functions 
	##description<<
	## Barplot or scatterplot of objective function metrics. A barplot is created if only one metric is defined in \code{which}. A scatterplot is created if two metrics are defined.
	
	x,
	### an object of class \code{\link{ObjFct}}
	
	which=c("Cor", "KS", "AE"), 
	### Which objective function metrics should be plotted? If one metric is defined, a barplot will be plotted. If more than one metric is defined, the first metric will be used for the x-axis and the second metric for y-axis of a scatterplot. If a third metric is specified, point sizes in the scatterplot will be scaled.
	
	alpha = 0.1,
	### significance level to plot objetive functions that are not significant different from the optimum with different point symbols. This only works if the the selected objective functions in which have a p-value. Set to NULL to avoid this option. 
	
	cols = "black",
	### colors for the groups
	
	pch = NULL,
	### point symbols for scatterplots or for pointsplots (instead of barplot)
	
	legend = TRUE,
	### plot a legend for the significance point symbols?
	
	xlab = NULL,
	### label for x-axis
	
	ylab = NULL,
	### label for y-axis
	
	xlim = NULL,
	### limits for x-axis
	
	ylim = NULL,
	### limits for y-axis
	
	txt = NULL,
	### text for the points
	
	add = FALSE,
	### add plot to an existing plot? This only works for scatterplots.
	
	...
	### further arguments to \code{\link{plot}}
	
	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{ObjFct}}, \code{\link{TaylorPlot}}, \code{\link{ScatterPlot}}, \code{\link{WollMilchSauPlot}}
) {

   of <- x
   x <- of[[match(which[1], names(of))]]
   
   # optimum
   suppressWarnings(opt <- ObjFct(1:10, 1:10))
   xopt <- opt[[match(which[1], names(opt))]]

   if (is.null(xlab)) xlab <- attr(of, "longnames")[match(which[1], names(of))]
         
   # text for groups
   if (is.null(txt)) {
      txt <- ""
      if (length(x) > 1) txt <- attr(of, "groupnames")
   }
      
   # make barplot
   #-------------
   
   if (length(which) == 1) {
      if (is.null(ylim)) ylim <- range(c(0, x, xopt), na.rm=TRUE)
      if (!is.null(pch)) {
         bp <- barplot(x, names="", ylim=ylim, ylab=xlab, plot=!add, col="white", border="white")
      } else {
         bp <- barplot(x, names="", ylim=ylim, ylab=xlab, plot=!add)
      }
      if (!add) {
         box()
         abline(h=xopt)
         srt <- 0
         if (length(txt) > 4) srt <- 20
         par(xpd=TRUE)
         pos <- ylim[1] - diff(ylim) * 0.05
         text(bp[,1], pos, txt, srt=srt, pos=1)
         par(xpd=FALSE)
      } 
      if (!add & !is.null(pch)) abline(v=bp[,1]+0.6, col="darkgrey")
      if (add | !is.null(pch)) {
         points(bp[,1], x, col=cols, pch=pch)
         points(bp[,1], x, col=cols, type="b", lty=1, lwd=0.4, pch=pch)
      }

   
   } else {
   
   # make scatterplot
   #-----------------

      y <- of[[match(which[2], names(of))]]
      yopt <- opt[[match(which[2], names(opt))]]
      
      # axis limits
      if (is.null(xlim)) xlim <- range(c(x, xopt), na.rm=TRUE)
      if (is.null(ylim)) ylim <- range(c(y, yopt), na.rm=TRUE)
      if (is.null(ylab)) ylab <- attr(of, "longnames")[match(which[2], names(of))]
      
      # don't plot p-values if point symbols are provided
      if (is.null(pch)) {
         pch <- rep(15, length(x))
      } else {
         alpha <- NULL
         legend <- FALSE
      }
      
      # plot p-values? defines point symbols
      if (!is.null(alpha)) {
         is.corx <- which[1] == "Cor" | which[1] == "Spearman"
         is.cory <- which[2] == "Cor" | which[2] == "Spearman"
         boolx <- grep(paste0(which[1], ".pval"), names(of))
         booly <- grep(paste0(which[2], ".pval"), names(of))
         if (length(boolx) == 1) { # x has p-value
            xp <- of[[boolx]]
            # optimum has p-value = 0
            if (opt[[boolx]] == 0 & !is.corx) pch[xp <= alpha] <- 25 
            if (opt[[boolx]] == 0 & is.corx) pch[xp <= alpha & x > 0] <- 25 
            # optimum has p-value = 1
            if (opt[[boolx]] == 1) pch[xp >= (1 - alpha)] <- 25
         }
         if (length(booly) == 1) { # y has p-value
            yp <- of[[booly]]
            # optimum has p-value = 0
            if (opt[[booly]] == 0 & !is.cory) pch[yp <= alpha] <- 24 
            if (opt[[boolx]] == 0 & is.cory) pch[xp <= alpha & y > 0] <- 24 
            # optimum has p-value = 1
            if (opt[[booly]] == 1) pch[yp >= (1 - alpha)] <- 24
         }
         if (length(boolx) == 1 & length(booly) == 1) { # x and y have p-values
            if (opt[[booly]] == 0 & opt[[boolx]] == 0) pch[yp <= alpha & xp <= alpha] <- 23 
            if (opt[[booly]] == 0 & opt[[boolx]] == 1) pch[yp <= alpha & xp >= (1 - alpha)] <- 23 
            if (opt[[booly]] == 1 & opt[[boolx]] == 0) pch[yp >= (1 - alpha) & xp <= alpha] <- 23 
            if (opt[[booly]] == 1 & opt[[boolx]] == 1) pch[yp >= (1 - alpha) & xp >= (1 - alpha)] <- 23 
            if (is.corx) pch[xp <= alpha & x < 0] <- 15
            if (is.cory) pch[yp <= alpha & y < 0] <- 15
         }

         lgd.pch <- c(NA, 15)
         lgd.txt <- c("difference to optimum", paste0("significant (alpha = ", alpha, ")"))
         if (any(pch == 25) | any(pch == 24) | any(pch == 23)) {        
            lgd.txt <- c(lgd.txt, "not significant for:")
            lgd.pch <- c(lgd.pch, NA)
            if (any(pch == 25)) lgd.txt <- c(lgd.txt, which[1])
            if (any(pch == 25)) lgd.pch <- c(lgd.pch, 25)
            if (any(pch == 24)) lgd.txt <- c(lgd.txt, which[2])
            if (any(pch == 24)) lgd.pch <- c(lgd.pch, 24)
            if (any(pch == 23)) lgd.txt <- c(lgd.txt, paste(which[1], "+", which[2]))
            if (any(pch == 23)) lgd.pch <- c(lgd.pch, 23)
         }
      }
      
      # prepare legend for p-value
      if (!is.null(alpha) & legend & any(pch != 15)) {
         pos <- "top"
         if (xopt == 1 & yopt == 0) pos <- "topright"
         if (xopt == 0 & yopt == 0) pos <- "bottomleft"
         if (xopt == 1 & yopt == 1) pos <- "topleft"
         if (xopt == 0 & yopt == 1) pos <- "bottomright"
         if (grepl("top", pos)) ylim[2] <- ylim[2] + diff(ylim) * 0.15
         if (grepl("bottom", pos)) ylim[1] <- ylim[1] - diff(ylim) * 0.15
      }
      
      cex <- 1
      if (!is.na(which[3])) {
         z <- of[[match(which[3], names(of))]]
         zopt <- opt[[match(which[3], names(opt))]]
         zopt[is.na(zopt)] <- 0
         z <- c(zopt, z)
         if (zopt == 0) cex <- MinMaxNorm(abs(z), c(0.5, 1.5))[-1]
         if (zopt == 1) cex <- MinMaxNorm(z*-1, c(0.5, 1.5))[-1]
      }
      
      # make plot
      if (!add) plot(x, y, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, pch=pch, col=cols, bg=cols, cex=cex)
      if (yopt == 0 & !add) abline(h=0)
      if (xopt == 0 & !add) abline(v=0)
      if (add) points(x, y, pch=pch, col=cols, bg=cols, cex=cex)
#      p <- 1
#      if (length(txt) > 10) p <- rep(1:4, length=length(txt))
      text(x, y, txt, col=cols, pos=1, cex=0.6)
      if (!add) { # optimum
         points(xopt, yopt, pch=4) 
         text(xopt, yopt, "Optimum", pos=3, cex=0.6)
      }
      
      if (!is.null(alpha) & legend & any(pch != 15)) legend(pos, lgd.txt, pch=lgd.pch, col="black", pt.bg="black", cex=0.8)
   } # end if else 
}, ex=function() {

# create some data
obs <- 1:300
sim <- obs * c(rnorm(100, 1, 0.1), rnorm(100, 1, 0.3), rnorm(100, 1, 0.5))
groups <- c(rep("subset 1", 100), rep("subset 2", 100), rep("subset 3", 100))
ScatterPlot(sim, obs, groups, objfct=TRUE)

of <- ObjFct(sim, obs, groups)
of 

# default plot
plot(of)

# barplots with only one metric
plot(of, "Cor")
plot(of, "AE")
plot(of, "MEF")

# options for the scatterplot:
plot(of, c("Cor", "RMSE", "MEF"))
plot(of, c("Spearman", "KS"), alpha=0.05)
plot(of, c("Spearman", "KS"), alpha=NULL)
plot(of, cols=c("black", "red", "blue", "green"))
plot(of, c("Cor", "FV"), pch=0:4)

})




