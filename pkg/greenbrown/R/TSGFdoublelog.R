TSGFdoublelog <- structure(function(
	##title<< 
	## Temporal smoothing and gap filling using double logisitic functions
	##description<<
	## This function fills gaps and smoothes a time series by fitting for each year a double logisitic function. Two definitions for the shape of the double logistic function are available: 'Elmore' fits a function according to (Elmore et al. 2012) and 'Beck' fits a according to (Beck et al. 2006). If the time series has no Seasonality, double logistic fitting will be not performed but smoothing and interpolation will be done using \code{\link{TSGFspline}}.
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values by using the logistic fit function?
	
	method = c("Elmore", "Beck"),
	### Which kind of double logistic curve should be used? 'Elmore' (Elmore et al. 2012) or 'Beck' (Beck et al. 2006).
	
	backup = NULL,
	### Which backup algorithm should be used for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"
	
	...
	### further arguments (currently not used)
	
	##references<< 
	## Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.
	## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.	
	
	##seealso<<
	## \code{\link{TsPP}}, \code{\link{Phenology}}

) {

	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	freq <- frequency(Yt)
	start <- start(Yt)
	end <- end(Yt)
	mn <- min(Yt, na.rm=TRUE)
	mx <- max(Yt, na.rm=TRUE)
	
	# output time steps in case of interpolation
	if (interpolate) {
		tout <- seq(1, freq, length=365)
		freq.out <- 365
	} else {
		tout <- seq(1, freq, length=freq)
		freq.out <- freq
	}	
		
	if (sum(Seasonality(Yt)) < 3) {	# decide for no seasonality if less than 3 methods say seasonality
		# run backup method if time series has no seasonality
		if (is.null(backup)) {
			Yt1 <- ts(NA, start=start, end=c(end[1], freq.out), frequency=freq.out)
			message("TSGFdoublelog: Time series has no seasonality. No smoothing performed because no backup algorithm choosen.")	
		} else {
			Yt1 <- do.call(backup, list(Yt=Yt, interpolate=interpolate, ...))
			message("TSGFdoublelog: Time series has no seasonality. Using", backup, "as smoothing, gap filling and interpolation backup algorithm instead of double logistic fit.")			
		}

	} else {
		# fit double logistic curve for each year
		method <- method[1]
		if (method == "Elmore") .fun <- FitDoubleLogElmore
		if (method == "Beck") .fun <- FitDoubleLogBeck
		Yt1 <- aggregate(Yt, FUN=.fun, weighting=TRUE, return.par=FALSE, tout=tout)	
		Yt1 <- ts(Yt1, start=start, end=c(end[1], freq.out), frequency=freq.out)

		# remove outliers
		Yt1[Yt1 < mn] <- mn
		Yt1[Yt1 > mx] <- mx
		
		# plot(Yt)
		# lines(Yt1, col="blue")	
	}
	return(Yt1)
	### The function returns a gap-filled and smoothed version of the time series.
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce random gaps 
gaps <- ndvi
gaps[runif(100, 1, length(ndvi))] <- NA
plot(gaps)

# do smoothing and gap filling
tsgf1 <- TSGFdoublelog(gaps, method="Elmore")
tsgf2 <- TSGFdoublelog(gaps, method="Beck")
plot(gaps)
lines(tsgf1, col="red")
lines(tsgf2, col="blue")

# compare original data with gap-filled data
plot(ndvi[is.na(gaps)], tsgf1[is.na(gaps)], col="red")
points(ndvi[is.na(gaps)], tsgf2[is.na(gaps)], col="blue")
abline(0,1)
r <- c(cor(ndvi[is.na(gaps)], tsgf1[is.na(gaps)], use="pairwise.complete.obs"), cor(ndvi[is.na(gaps)], tsgf2[is.na(gaps)], use="pairwise.complete.obs"))
legend("topleft", paste(c("Elmore Cor =", "Beck Cor ="), round(r, 3)), text.col=c("red", "blue"))

})



