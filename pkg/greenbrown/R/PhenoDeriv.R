PhenoDeriv <- structure(function(
	##title<< 
	## Method 'Deriv' to calculate phenology metrics
	##description<<
	## This function implements the derivative method for phenology. This is rather an internal function; please use the function \code{\link{Phenology}} to apply this method.
	
	x, 
	### seasonal cycle of one year
			
	min.mean = 0.1,
	### minimum mean annual value in order to calculate phenology metrics. Use this threshold to suppress the calculation of metrics in grid cells with low average values	
	
	calc.pheno = TRUE,
	### calculate phenology metrics or return NA?
	
	plot = FALSE,
	### plot results?
	
	...
	### further arguments (currently not used)
		
	##seealso<<
	## \code{\link{Phenology}}

) {

	if (all(is.na(x))) return(c(sos=NA, eos=NA, los=NA, pop=NA, pot=NA, mgs=NA, rsp=NA, rau=NA, peak=NA, trough=NA, msp=NA, mau=NA))

	# get statistical values
	n <- length(x)
	avg <- mean(x, na.rm=TRUE)
	x2 <- na.omit(x)
	avg2 <- mean(x2[x2 > min.mean], na.rm=TRUE)
	peak <- max(x, na.rm=TRUE)
	trough <- min(x, na.rm=TRUE)
	ampl <- peak - trough
	
	# get position of seasonal peak and trough
	pop <- median(which(x == max(x, na.rm=TRUE)))
	pot <- median(which(x == min(x, na.rm=TRUE)))
		
	# return NA if amplitude is too low or time series has too many NA values
	if (!calc.pheno) {
		if (avg < min.mean) { # return for all metrics NA if mean is too low
			return(c(sos=NA, eos=NA, los=NA, pop=NA, pot=NA, mgs=NA, rsp=NA, rau=NA, peak=NA, trough=NA, msp=NA, mau=NA))
		} else { # return at least annual average and peak if annual mean > min.mean
			return(c(sos=NA, eos=NA, los=NA, pop=pop, pot=pot, mgs=avg2, rsp=NA, rau=NA, peak=peak, trough=NA, msp=NA, mau=NA))
		}
	}
		
	# calculate derivative
	xd <- c(NA, diff(x))
	
	# get SOS and EOS 
	soseos <- 1:length(x)
	rsp <- max(xd, na.rm=TRUE)
	rau <- min(xd, na.rm=TRUE)
	sos <- median(soseos[xd == rsp], na.rm=TRUE)
	eos <- median(soseos[xd == rau], na.rm=TRUE)
	los <- eos - sos
	los[los < 0] <- n + (eos[los < 0] - sos[los < 0])
	
	# get MGS
	if (sos < eos) {
		mgs <- mean(x[sos:eos], na.rm=TRUE)
	} else {
		mgs <- mean(x[c(1:eos, sos:n)], na.rm=TRUE)
	}
		
	# get MSP, MAU
	msp <- mau <- NA
	if (!is.na(sos)) {
		id <- (sos-10):(sos+10)
		id <- id[(id > 0) & (id < n)]
		msp <- mean(x[id], na.rm=TRUE)
	}
	if (!is.na(eos)) {
		id <- (eos-10):(eos+10)
		id <- id[(id > 0) & (id < n)]
		mau <- mean(x[id], na.rm=TRUE)
	}
	metrics <- c(sos=sos, eos=eos, los=los, pop=pop, pot=pot, mgs=mgs, rsp=rsp, rau=rau, peak=peak, trough=trough, msp=msp, mau=mau)
	
	if (plot) {
		PlotPhenCycle(x, metrics=metrics, ...)
	}
		
	return(metrics)
	### The function returns a vector with SOS, EOS, LOS, POP, MGS, RSP, RAU, PEAK, MSP and MAU. 
}, ex=function() {
# perform time series preprocessing from first year of data
x <- TsPP(ndvi, interpolate=TRUE)[1:365]
plot(x)

# calculate phenology metrics for first year
PhenoDeriv(x, plot=TRUE)

})


