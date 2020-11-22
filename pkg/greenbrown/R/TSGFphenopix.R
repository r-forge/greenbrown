TSGFphenopix <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling using phenopix
	##description<<
	## Time series smoothing and gap filling using fitting methods as provided in the \code{\link{greenProcess}} function of the \code{phenopix} package. Function fits are performed for each year separately for which \code{\link{PhenopixMY}} is used. 
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values?
	
	fit = "spline" ,
	### fitting function to be applied, available options are: spline, beck, elmore, klosterman, gu (see \code{\link{greenProcess}})
	
	...
	### further arguments (currently not used)
	
	##seealso<<
	## \code{\link{PhenopixMY}}, \code{\link{TsPP}}

) {

	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	freq <- frequency(Yt)
	n <- length(Yt)
	start <- start(Yt)
	end <- end(Yt)
	mn <- min(Yt, na.rm=TRUE)
	mx <- max(Yt, na.rm=TRUE)

	# apply phenopix
	ppix <- PhenopixMY(Yt, fit=fit)
	
	# convert to ts
	Yt2 <- AllTsteps(ppix$fit, by="day", exclude.feb29=TRUE)

   # convert to zoo and aggregate to monthly in case interpolate == FALSE
	if (freq == 12 & !interpolate) {
	   ym <- format(time(Yt2), "%Y-%m")
	   Yt3 <- aggregate(Yt2, ym, mean, na.rm=TRUE)
	   start2 <- as.numeric(format(time(Yt2)[1], "%Y"))
	   Yt4 <- window(ts(Yt3, start=start2, frequency=freq), start=start, end=end)
	} else {
	   start2 <- as.numeric(format(time(Yt2)[1], "%Y"))
	   Yt4 <- window(ts(Yt2, start=start2, frequency=365), start=start, end=c(end[1], 365))
	}
	
	# remove outliers
	Yt4[Yt4 < mn] <- mn
	Yt4[Yt4 > mx] <- mx
	
	return(Yt4)
	### The function returns a gap-filled and smoothed version of the time series.
}, ex=function() {
# # introduce random gaps 
# gaps <- ndvi
# gaps[runif(100, 1, length(ndvi))] <- NA
# plot(gaps)
# 
# # do smoothing and gap filling
# tsgf <- TSGFphenopix(gaps, fit="spline")
# plot(gaps)
# lines(tsgf, col="red")
# 
# # compare original data with gap-filled data
# plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled")
# abline(0,1)
# r <- cor(ndvi[is.na(gaps)], tsgf[is.na(gaps)])
# legend("topleft", paste("Cor =", round(r, 3)))
# 
# # compare spline from phenopix with TSGFspline
# spl <- TSGFspline(gaps)
# plot(gaps)
# lines(tsgf, col="red")
# lines(spl, col="blue")
# legend("topleft", c("TSGFphenopix.spline", "TSGFspline"), text.col=c("red", "blue"))
# # Note that the differences originate from the fact that TSGFspline is applied on 
# # the full time series whereas spline within phenopix is applied for each year 
# # separetely. Yearly fits for TSGFphenopix.spline are afterwards combined to a full 
# # time series. This can cause jumps or peaks between two years. Thus, TSGFspline is 
# # the better choice for multi-year time series. This is also seen in cross-validation:
# plot(ndvi[is.na(gaps)], tsgf[is.na(gaps)], xlab="original", ylab="gap filled", col="red")
# points(ndvi[is.na(gaps)], spl[is.na(gaps)], col="blue")
# abline(0,1)
# r <- cor(cbind(ndvi[is.na(gaps)], tsgf[is.na(gaps)], spl[is.na(gaps)]))
# lgd <- paste(c("TSGFphenopix.spline", "TSGFspline"), "Cor =", round(r[1,2:3], 3))
# legend("topleft", lgd, text.col=c("red", "blue"))
# 
# # Other fits wihtin phenopix might be usefull but are rather computationally expensive:
# tsgf <- TSGFphenopix(gaps, fit="klosterman")
# plot(gaps)
# lines(tsgf, col="red")


})



