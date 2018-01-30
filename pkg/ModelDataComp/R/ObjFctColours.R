ObjFctColours <- structure(function(
	##title<< 
	## Class breaks and colours for objective functions
	##description<<
	## The function computes class breaks and colour palettes for \code{\link{ObjFct}} metrics 
	
	x, 
	### Either an object of class \code{\link{ObjFct}} or a vector with values of an objective function metrics
	
	which = NULL, 
	### which objective function metric should be used to create colours and class breaks?
	
	cols = NULL,
	### vector of colours that should be used
   
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{ObjFct}}
) { 
   if ("ObjFct" %in% class(x)) {
      if (is.null(which)) which <- "Cor"
      x <- unlist(x[grep(which, names(x))[1]])
   }

   # optimum objective function
   of.best <- ObjFct(1:10, 1:10)
   of.best <- unlist(of.best[grep(which, names(of.best))[1]])
   
   # worst obj function
   of.bad <- NA
   if (which == "Cor" | which == "Spearman") of.bad <- -1
   if (which == "R2" | which == "IoA") of.bad <- 0
   if (which == "KS") of.bad <- 1
   if (which == "FV") of.bad <- c(-2, 2)
   
   # all objective functions
   of <- na.omit(c(x, of.bad, of.best))
   
   # rank of objective function
   if (any(is.na(of.bad))) {
      rof <- abs(of - of.best)
      rx <- abs(x - of.best)
   } else {
      rof <- of
      rx <- x
   }
   
   # create breaks 
   brks <- pretty(rof, min.n=10, n=15)
      
   # create colors
   hasno.cols <- is.null(cols)
   if (hasno.cols) cols <- rev(c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4"))
   cols0 <- colorRampPalette(cols)
   cols0 <- cols0(length(brks)-1)
   if (hasno.cols & of.best > 0 & !any(is.na(of.bad))) cols0 <- rev(cols0)
   cols <- cols0[cut(rx, brks)]
   return(list(x=x, cols=cols, brks=brks, cols0=cols0))
   ### A list with class breaks and colours
}, ex=function() {

# create some data and compute objective functions
obs <- 1:100 # 'observations'
sim <- obs * c(rnorm(25, 1, 0.2), rnorm(25, 1, 0.5), rnorm(25, 1.5, 1), rnorm(25, -0.3, 0.3))
groups <- c(rep("subset 1", 25), rep("subset 2", 25), rep("subset 3", 25), rep("subset 4", 25))
ScatterPlot(obs, sim, groups)
of <- ObjFct(sim, obs, groups=groups)
of

# create colours and class breaks: default for correlation
cols <- ObjFctColours(of)
barplot(of[["Cor"]], col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# create colours and class breaks for RMSE
cols <- ObjFctColours(of, "RMSE")
barplot(of[["RMSE"]], col=cols$cols)
legend("top", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# create colours and class breaks based on vectors
ioa <- of[["IoA"]]
cols <- ObjFctColours(ioa, "IoA")
barplot(ioa, col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

# use different colours
cols <- ObjFctColours(ioa, "IoA", cols=c("red", "green", "blue"))
barplot(ioa, col=cols$cols)
legend("topright", as.character(cols$x), fill=cols$cols, title="Colour for each value")
legend("topleft", as.character(cols$brks[-1]), fill=cols$cols0, title="Colour palette")

})

