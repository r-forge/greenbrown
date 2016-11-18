MakeFig <- structure(function(
	##title<< 
	## fig positions for graphics
	##description<<
	## Calculates positions for figure that consist of multiple panels
	
	nfig, 
	### number of figures
	
	border=c(0, 1, 0, 1), 
	### relative graphic borders in which the figures should be placed
	
	nrow=NULL, 
	### number of rows to arrange the figures
	
	ncol=NULL
	### number of cols to arrange the figures
	
	) {
	# estimate optimal number of rows and columns
	if (is.null(nrow) | is.null(ncol)) {
		# pre-defined matrices for maximum 16 elements
		dim.df <- data.frame(
			n = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
			nrow = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4),
			ncol = c(1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5)
		)
		
		if (nfig < 20) {
			nrow <- dim.df$nrow[nfig]
			ncol <- dim.df$ncol[nfig]
		} else {
			m <- lm(nrow ~ n, dim.df)
			nrow <- ceiling(predict(m, data.frame(n=nfig)))
			m <- lm(ncol ~ n, dim.df)
			ncol <- ceiling(predict(m, data.frame(n=nfig)))
		}
	}
			
	xdiff <- (border[2] - border[1]) / ncol
	x1 <- seq(border[1], border[2] - xdiff, by=xdiff) 
	x2 <- seq(border[1] + xdiff, border[2], by=xdiff)
	ydiff <- (border[4] - border[3]) / nrow
	y1 <- seq(border[4], border[3] + ydiff, by=ydiff*-1) 
	y2 <- seq(border[4] - ydiff, border[3], by=ydiff*-1) 	
	
	fig.l <- vector("list", nfig)
	i <- 1
	for (y in 1:nrow) {
		for (x in 1:ncol) {
			fig.l[[i]] <- c(x1[x], x2[x], y2[y], y1[y])
			i <- i + 1
		}
	}
	return(list(fig=fig.l, nrow=nrow, ncol=ncol))
	### A list with positions for each figure and number of rows and columns
})
