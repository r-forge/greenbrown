TsPP <- structure(function(
	##title<< 
	## Pre-processing of time series
	##description<<
	## This function can be used for pre-processing of time series before the analyzing phenology or trends. The pre-processing involves the following steps: 
	## \itemize{ 
	## \item{ Step 1. Filling of permanent gaps. Values that are missing in each year will be filled using the function \code{\link{FillPermanentGaps}}.  }
	## \item{ Step 2. Temporal smoothing, gap filling and interpolation. The time series will be smoothed and remaining gaps will be filled. Optionally, time series will be interpolated to daily values. }
	## }
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.

	fpg = FillPermanentGaps,
	### Filling of permanent gaps: If NULL, permanent gaps will be not filled, else the function \code{\link{FillPermanentGaps}} will be applied.
	
	tsgf = TSGFspline,
	### Temporal smoothing and gap filling: Function to be used for temporal smoothing, gap filling and interpolation of the time series. If NULL, this step will be not applied. Otherwise a function needs to be specified. Exisiting functions that can be applied are \code{\link{TSGFspline}}, \code{\link{TSGFssa}}, \code{\link{TSGFdoublelog}}  
	
	interpolate = FALSE,
	### Should the smoothed and gap filled time series be interpolated to daily values?
	
	min.gapfrac = 0.2,
	### How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.	
	
	lower = TRUE,
	### For filling of permanent gaps: fill lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)
	
	fillval = NA,
	### For filling of permanent gaps: constant fill values for gaps. If NA the fill value will be estimated from the data using fun. 
	
	fun = min,
	### For filling of permanent gaps: function to be used to compute fill values. By default, minimum.
	
	backup = NULL,
	### Which backup algorithm should be used instead of TSGFdoublelog for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"	
	
	check.seasonality = 1:3,
	### Which methods in \code{\link{Seasonality}} should indicate TRUE (i.e. time series has seasonality) in order to calculate phenology metrics? 1:3 = all methods should indicate seasonality, Set to NULL in order to not perform seasonality checks.
		
	...
	### further arguments (currently not used)
	
	##seealso<<
	## \code{\link{FillPermanentGaps}}

) {

	if (class(Yt) != "ts") stop("TsPP: Yt should be class of 'ts'.")
		
	# get time series properties
	start <- start(Yt)
	end <- end(Yt)
	
	# Step 1: fill permanent gaps
	if (!is.null(fpg)) {
		Pg <- IsPermanentGap(Yt, lower=lower, min.gapfrac=min.gapfrac)
		Yt1 <- FillPermanentGaps(Yt, min.gapfrac=min.gapfrac, fillval=fillval, fun=fun)
	} else {
		Yt1 <- Yt
	}
	
	# Step 2: perform smoothing, gap-filling and interpolation
	if (!is.null(tsgf)) {
		Yt2 <- tryCatch({
			do.call(tsgf, list(Yt=Yt1, interpolate=interpolate, backup=backup, check.seasonality=check.seasonality, ...))
		}, error = function(e) {
			message(paste("TsPP: ", tsgf, " produced an error with Yt1: c(", paste(Yt1, collapse=", "), ")", sep=""))
			message(paste("      returning NA for this time series."))		
			if (interpolate) ts(NA, start=start, end=c(end[1], 365), frequency=365)
			if (!interpolate) ts(NA, start=start, end=end, frequency=frequency(Yt))
		}, finally = function(x) {
			if (interpolate) ts(NA, start=start, end=c(end[1], 365), frequency=365)
			if (!interpolate) ts(NA, start=start, end=end, frequency=frequency(Yt))
		})
		
	} else {
		Yt2 <- Yt1
	}

	return(Yt2)
	### pre-processed time series
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce systematic gaps in winter and random gaps
gaps <- ndvi
gaps[runif(50, 1, length(ndvi))] <- NA
gaps[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12] <- NA
plot(gaps)

# perform pre-processing of time series using different methods 
pp.lin <- TsPP(gaps, tsgf=TSGFlinear) # linear interpolation + running median
pp.spl <- TsPP(gaps, tsgf=TSGFspline) # smoothing splines
pp.beck <- TsPP(gaps, tsgf=TSGFdoublelog, method="Beck") # Beck et al. (2006)
pp.elmore <- TsPP(gaps, tsgf=TSGFdoublelog, method="Elmore") # Elmore et al. (2012)

plot(gaps)
cols <- rainbow(5)
lines(pp.lin, col=cols[1])
lines(pp.spl, col=cols[2])
lines(pp.beck, col=cols[3])
lines(pp.elmore, col=cols[4])

data.df <- ts.union(time(gaps), orig=ndvi, pp.lin, pp.spl, pp.beck, pp.elmore)
plot(data.df)
cor(na.omit(data.df[is.na(gaps),]))

})



