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
   op <- par()
   if (is.null(mfrow)) {
      nplot <- length(x$x.names) + 1
      fig <- MakeFig(nplot)
      mfrow <- c(fig$nrow, fig$ncol)
   }

   par(mfrow=mfrow)

   # histogram
   brks <- hist(x$data$y, xlab=ylab, main="", col="grey") 
   box()
   if (is.null(main)) main <- x$eq
   mtext(main, 3, 0, font=2, outer=TRUE)
   if (!is.null(labels)) legend("topleft", labels[1], bty="n")
   
   cols0 <- "black"
   if (any(x$with.group)) cols0 <- yarrr::piratepal("basel")

   # plot response functions
   if (ncol(x$data) > 1) {
      if (is.null(plot.order)) plot.order <- 1:length(x$x.names)
      count <- 2
      for (i in plot.order) {
         xvar <- x$data[, grep(paste0("x.", x$x.names[i]), colnames(x$data))[1]]
         if (length(xvar > 500)) xvar <- seq(min(xvar, na.rm=TRUE), max(xvar, na.rm=TRUE), length=500)
         o <- order(xvar)
         xlim <- range(xvar, na.rm=TRUE)
         m <- c(paste0("f.", x$x.names[i]), paste0("f.", x$x.names[i], ".", x$group.names))
         g <- na.omit(match(m, colnames(x$data)))
         cols <- "black"
         if (length(g) > 1) cols <- cols0[1:length(x$group.names)]
         cols.lgd <- NULL
         for (j in 1:length(g)) {
         
            if (length(g) == 1) {
               pmx <- x$par$par[match(paste0(x$x.names[i], ".max"), x$par$names)]
               psl <- x$par$par[match(paste0(x$x.names[i], ".sl"), x$par$names)]
               px0 <- x$par$par[match(paste0(x$x.names[i], ".x0"), x$par$names)]
               pmn <- x$par$par[match(paste0(x$x.names[i], ".min"), x$par$names)]
            }
            if (length(g) > 1) {
               pmx <- x$par$par[match(paste0(x$x.names[i], ".max.", x$group.names[j]), x$par$names)]
               psl <- x$par$par[match(paste0(x$x.names[i], ".sl.", x$group.names[j]), x$par$names)]
               px0 <- x$par$par[match(paste0(x$x.names[i], ".x0.", x$group.names[j]), x$par$names)]
               pmn <- x$par$par[match(paste0(x$x.names[i], ".min.", x$group.names[j]), x$par$names)]
            }
            yvar <- SofiaLogistic(c(pmx, psl, px0, pmn), xvar)
            yvar[yvar > 1] <- 1
            yvar[yvar < 0] <- 0
                  
            # plot response function
            if (j == 1) {
               ylim <- c(0, max(x$data[, g], na.rm=TRUE))
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
            cols.lgd <- c(cols.lgd, rev(cols)[j])
            
            # plot parameter values
            if (j == length(g)) {
               if (length(g) > 1) {
                  g2 <- c(
                     match(paste(x$x.names[i], "max", x$group.names, sep="."), x$par$names),
                     match(paste(x$x.names[i], "sl", x$group.names, sep="."), x$par$names),
                     match(paste(x$x.names[i], "x0", x$group.names, sep="."), x$par$names)
                  )
               } else {
                  g2 <- c(grep(paste0(x$x.names[i], ".max"), x$par$names), grep(paste0(x$x.names[i], ".sl"), x$par$names), grep(paste0(x$x.names[i], ".x0"), x$par$names))
               }
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
               legend("topright", txt, ncol=ncol, bty="n", cex=0.7, text.col=cols.lgd)
            }
         }
         if (!is.null(labels)) {
            legend("topleft", labels[count], bty="n")
            count <- count + 1
         }
         
      }
   }
   on.exit(par(op))
}, ex=function() {

  # get data
  data(firedata)
  
  # predictor variables
  train <- firedata$train == 1 # use training data
  xvars.df <- data.frame(
    NLDI = firedata$NLDI[train],
    CRU.WET.orig = firedata$CRU.WET.orig[train],
    Liu.VOD.annual = firedata$Liu.VOD.annual[train],
    GIMMS.FAPAR.pre = firedata$GIMMS.FAPAR.pre[train],
    CRU.DTR.orig = firedata$CRU.DTR.orig[train]
  )
  
  # observed data
  obs <- firedata$GFED.BA.obs[train]
  regid <- firedata$regid[train]
  
  # Which x variable should depend on land cover?
  per.group <- c(FALSE, TRUE, TRUE, TRUE, TRUE)
  
  # land cover
  area <- data.frame(
    Tree = firedata$CCI.LC.Tree[train],
    Shrub = firedata$CCI.LC.Shrub[train],
    HrbCrp = firedata$CCI.LC.HrbCrp[train]
  )
  
  # define parameters (from Forkel et al. 2016, Fig. 1)
  sofiapar <- SofiaPar(colnames(xvars.df), colnames(area), per.group=per.group, 
                       par.act=c(1.9, 0, 780, 1, # for PopDens
                                 0.3, 1.1, -5.3, 0, 0, 0, 8.9, 0.54, -23, -39, 13, -16, # for CRU.DTR
                                 0.13, 3, 0.53, 0, 0, 0, 0.35, -0.44, 0.36, -1.2, -4.8, -45, # for CRU.WET
                                 -0.7, 18, -1.5, 0, 0, 0, 22, 11, -17, -2.3, 0.64, 1,  # for GIMMS.FAPAR
                                 1.9, 3, -0.36, 0, 0, 0, -21, 68, -38, 0.35, 0.31, 0.11) # for Liu.VOD
  )
  
  # run model
  sf <- Sofia(xvars.df, area, per.group=per.group, sofiapar=sofiapar)
  plot(sf) 
  

})
