plot.Sofia <- structure(function(
	##title<< 
	## plot a Sofia object
	##description<<
	## Plots a \code{\link{Sofia}} object.
	
	x,
	### a 'Sofia' object
	
	ylab="y",
	### label for response variable
	
	mfrow = NULL,
	### number of rows and columns for the plot
	
	names = NULL,
	### names of the variables in the response functions
	
	main = NULL,
	### title of the plot
	
	plot.order = NULL,
	### Order for plotting of factors
	
	labels = paste0("(", letters, ")"),
	### Labels for subplots. Set to NULL to avoid labels.
	
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}
) { 
   if (is.null(mfrow)) {
      nplot <- length(x$x.names) + 1
      fig <- MakeFig(nplot)
      mfrow <- c(fig$nrow, fig$ncol)
   }

   par(mfrow=mfrow)

   # histogram
   brks <- hist(x$data$y, xlab=ylab, main="", col="grey", ...) 
   box()
   if (is.null(main)) main <- x$eq
   mtext(main, 3, 0, font=2, outer=TRUE)
   if (!is.null(labels)) legend("topleft", labels[1], bty="n")
   
   require(RColorBrewer)
   cols0 <- "black"
   if (any(x$with.group)) cols0 <- brewer.pal(9, "Set1")

   # plot response functions
   if (ncol(x$data) > 1) {
      if (is.null(plot.order)) plot.order <- 1:length(x$x.names)
      count <- 2
      for (i in plot.order) {
         xvar <- x$data[, grep(paste0("x.", x$x.names[i]), colnames(x$data))[1]]
         o <- order(xvar)
         xlim <- range(xvar, na.rm=TRUE)
         m <- c(paste0("f.", x$x.names[i]), paste0("f.", x$x.names[i], ".", x$group.names))
         g <- na.omit(match(m, colnames(x$data)))
         cols <- "black"
         if (length(g) > 1) cols <- cols0[1:length(x$group.names)]
         for (j in 1:length(g)) {
         
            if (length(g) == 1) {
               pmx <- x$par$par[match(paste0(x$x.names[i], ".max"), x$par$names)]
               psl <- x$par$par[match(paste0(x$x.names[i], ".sl"), x$par$names)]
               px0 <- x$par$par[match(paste0(x$x.names[i], ".x0"), x$par$names)]
            }
            if (length(g) > 1) {
               pmx <- x$par$par[match(paste0(x$x.names[i], ".max.", x$group.names[j]), x$par$names)]
               psl <- x$par$par[match(paste0(x$x.names[i], ".sl.", x$group.names[j]), x$par$names)]
               px0 <- x$par$par[match(paste0(x$x.names[i], ".x0.", x$group.names[j]), x$par$names)]
            }
            yvar <- Logistic(c(pmx, psl, px0), xvar)
            yvar[yvar > 1] <- 1
            yvar[yvar < 0] <- 0
                  
            # plot response function
            if (j == 1) {
               ylim <- range(x$data[, g])
               ylim[2] <- ylim[2] + diff(ylim) * 0.3
               
               if (is.null(names)) {
                  xlab <- x$x.names[i]
                  ylab <- paste0("f(", x$x.names[i], ")")
               } else {
                  xlab <- names[i]
                  ylab <- paste0("f(", names[i], ")")
               }
               
               plot(1, 1, type="n", ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, main="", yaxt="n")
               axis(2, seq(0, 1, by=0.2), seq(0, 1, by=0.2))
            }
            lines(xvar[o], yvar[o], col=rev(cols)[j], lwd=2, lty=1)
            
            # plot parameter values
            if (j == 1) {
               g2 <- c(grep(paste0(x$x.names[i], ".max"), x$par$names), grep(paste0(x$x.names[i], ".sl"), x$par$names), grep(paste0(x$x.names[i], ".x0"), x$par$names))
               txt0 <- laply(strsplit(x$par$names[g2], ".", fixed=TRUE), function(s) {
                  matrix(c(s, rep("", 20 - length(s))), nrow=1)
               })
               txt0 <- txt0[ , !apply(txt0, 2, AllEqual)]
               if (is.matrix(txt0)) {
                  #txt <- apply(txt, 1, function(s) paste(s, collapse="."))
                  txt <- txt0[,1]
                  txt.gr <- txt0[,2]
                  txt.gr[duplicated(txt.gr)] <- ""
                  txt <- paste0(txt.gr, " ", txt, " = ", signif(x$par$par[g2], 2))
                  txt <- apply(matrix(txt, nrow=length(x$group.names), 3, byrow=FALSE), 1, paste, collapse="")
               } else {
                  txt <- paste0(txt0, " = ", signif(x$par$par[g2], 2))
               }

              # txt <- as.character(signif(x$par$par[g2], 2))
               ncol <- 1
               if (length(g2) > 1) {
                  ncol <- 1
               }
               legend("topright", txt, ncol=ncol, bty="n", cex=0.7, text.col=rep(cols, length(g)))
            }
         }
         if (!is.null(labels)) {
            legend("topleft", labels[count], bty="n")
            count <- count + 1
         }
         
      }
  }

}, ex=function() {

# explanatory variables
sm <- 1:100
temp <- rnorm(100, 12, 10)
x <- cbind(sm, temp)

# fractional coverage of groups, e.g. plant functional types
tree <- runif(100, 0, 0.8)
grass <- 1 - tree
area <- cbind(tree, grass)

# with some more realisitc parameters:
par <- SofiaPar(colnames(x), per.group=c(TRUE, FALSE), group.names=c("tree", "grass"))
par$par <- c(1, 1, 20, 2, 1, -0.2, -0.1, 13, 10)
sf <- Sofia(x, area, per.group=c(TRUE, FALSE), sofiapar=par)
plot(sf)

})
