print.Phenology <- summary.Phenology <- structure(function(
	##title<< 
	## Print a summary of phenology metrics
	
	##description<< The function prints a summary the results from \code{\link{Phenology}}.
	
	x
	### Object of class 'Phenology' as returned from function \code{\link{Phenology}}
	
) {
	
	# print time series information
	cat("--- Phenology ----------------------------", "\n")
	cat("Calculate phenology metrics on time series", "\n")
	cat("------------------------------------------", "\n")
	cat("\n")
	cat("Method", "\n")
	if (is.null(x$tsgf)) {
		cat(" no smoothing and gap filling.", "\n")
	} else {
		cat(" - smoothing and gap filling : ", x$tsgf, "\n")	
		if (x$tsgf == "TSGFdoublelog") {
			cat(" - logistic function method  : ", x$method, "\n")
		}	
	}
	cat(" - summary approach          : ", x$approach, "\n")
	cat("\n")
	cat("Mean and standard deviation of annual metrics:", "\n")
	if (!x$calc.pheno) { 
		cat("Phenology metrics were not calculated.", "\n")
		cat("Seasonality was not detected by all methods:", "\n")
		cat(paste(names(x$seasonal), ": ", x$seasonal, ", ", sep=""), "\n")
	} else {
		cat("SOS    : ", signif(mean(x$sos, na.rm=TRUE), 2), "+-", signif(sd(x$sos, na.rm=TRUE), 2), "\n")
		cat("EOS    : ", signif(mean(x$eos, na.rm=TRUE), 2), "+-", signif(sd(x$eos, na.rm=TRUE), 2), "\n")
		cat("LOS    : ", signif(mean(x$los, na.rm=TRUE), 2), "+-", signif(sd(x$los, na.rm=TRUE), 2), "\n")
		cat("MSP    : ", signif(mean(x$msp, na.rm=TRUE), 2), "+-", signif(sd(x$msp, na.rm=TRUE), 2), "\n")
		cat("MAU    : ", signif(mean(x$mau, na.rm=TRUE), 2), "+-", signif(sd(x$mau, na.rm=TRUE), 2), "\n")
		cat("RSP    : ", signif(mean(x$rsp, na.rm=TRUE), 2), "+-", signif(sd(x$rsp, na.rm=TRUE), 2), "\n")
		cat("RAU    : ", signif(mean(x$rau, na.rm=TRUE), 2), "+-", signif(sd(x$rau, na.rm=TRUE), 2), "\n")
	}
	cat("POP    : ", signif(mean(x$pop, na.rm=TRUE), 2), "+-", signif(sd(x$pop, na.rm=TRUE), 2), "\n")
	cat("POT    : ", signif(mean(x$pot, na.rm=TRUE), 2), "+-", signif(sd(x$pot, na.rm=TRUE), 2), "\n")
	cat("MGS    : ", signif(mean(x$mgs, na.rm=TRUE), 2), "+-", signif(sd(x$mgs, na.rm=TRUE), 2), "\n")
	cat("PEAK   : ", signif(mean(x$peak, na.rm=TRUE), 2), "+-", signif(sd(x$peak, na.rm=TRUE), 2), "\n")
	cat("TROUGH : ", signif(mean(x$trough, na.rm=TRUE), 2), "+-", signif(sd(x$trough, na.rm=TRUE), 2), "\n")
	
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate phenology metrics
phen <- Phenology(ndvi)
phen
print(phen)

# plot phenology metrics
plot(phen)
	
})


plot.Phenology <- structure(function(
	##title<< 
	## Plot time series of phenology metrics
	
	##description<<
	## This is the standard plot function for results of the \code{\link{Phenology}} function. See \code{\link{plot.default}} for further specifications of basic plots.
	
	x,
	### Object of class 'Phenology' as returned from function \code{\link{Phenology}}
	
	type = c("sos", "eos", "pop"),
	### varaible names that should be plotted from the \code{\link{Phenology}} object
	
	ylab=NULL,
	### a title for the y axis
	
	ylim=NULL,
	### limits for y-axis
	
	add=FALSE,
	### add time series to exisiting plot?
	
	col="black",
	### line colors
	
	add.trend = TRUE,
	### add trend lines to phenology time series?
	
	...
	### Further arguments that can be passed \code{\link{plot.default}}
		
	##seealso<<
	## \code{\link{plot.default}}, \code{\link{plot.ts}}
) {
	start <- start(x$sos)
	end <- end(x$sos)
	freq <- frequency(x$sos)
	
	# type of plot -> axis limits and labels
	nms <- type
	if (is.null(ylim)) {
		a <- x[match(nms, names(x))]
		if (!AllEqual(unlist(a))) ylim <- range(a, na.rm=TRUE)
	}
	if (any(grepl("sos", type)) | any(grepl("eos", type)) | any(grepl("pop", type)) | any(grepl("pot", type))) {
		if (is.null(ylab)) ylab <- "DOY"
		if (is.null(ylim)) ylim <- c(1, 365)
	} else if (any(grepl("rsp", type)) | any(grepl("rau", type))) {
		if (is.null(ylab)) ylab <- "Rate"
		if (is.null(ylim)) ylim <- c(-1, 1)
	} else if (any(grepl("los", type))) {
		if (is.null(ylab)) ylab <- "LOS (days)"
		if (is.null(ylim)) ylim <- c(1, 365)
	} else {
		if (is.null(ylab)) ylab <- "NDVI"
		if (is.null(ylim)) ylim <- c(0, 1)
	}
		
	ytxt <- rep(NA, length(nms))
	for (m in 1:length(nms)) {
		i <- grep(nms[m], names(x)) 
		ts <- ts(x[[i]], start=start, freq=freq)
		plot.trend <- add.trend
		if (AllEqual(ts) | sum(!is.na(ts)) < 5) plot.trend <- FALSE
		
		# initalize graphic
		if (!add & m == 1) {
			xlim <- c(start[1], end[1]+1)
			plot(ts, type="n", xlab="", col=col, ylim=ylim, xlim=xlim, ylab=ylab) #, ...)
		}
		
		# plot time series with or without trend line
		if (plot.trend) {
			trd <- Trend(ts)
			plot(trd, symbolic=TRUE, add=TRUE, col=col) #, ...)
			ytxt[m] <- trd$trend[length(trd$trend)]
		} else {
			lines(ts, col=col, ...)
			ytxt[m] <- ts[length(ts)]
		}
	}
	
	text(x=end[1]+1, ytxt, nms, col=col, cex=1.2)

}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# calculate phenology metrics
phen <- Phenology(ndvi)
phen

# plot phenology metrics
plot(phen)
	
})