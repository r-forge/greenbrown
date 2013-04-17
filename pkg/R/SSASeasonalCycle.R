SSASeasonalCycle <- structure(function(
	##title<< 
	## Calculate a modulated seasonal cycle of a time series based on singular spectrum analysis (SSA)
	
	##description<<
	## The function calculates a seasonal cycle based on 1-D singular spectrum analysis (Golyandina et al. 2001) as implemented in the Rssa package. See \code{\link[Rssa]{ssa}} for details. 
	
	ts
	### univariate time series of class \code{\link[stats]{ts}}
	
	##references<<Golyandina, N., Nekrutkin, V. and Zhigljavsky, A. (2001): Analysis of Time Series Structure: SSA and related techniques. Chapman and Hall/CRC. ISBN 1584881941 
	
	##seealso<<
	## \code{\link{TrendSeasonalAdjusted}}, \code{\link[Rssa]{new.ssa}}, \code{\link[Rssa]{reconstruct}}
) {

	if (class(ts) != "ts") stop("ts should be of class ts.")
	require("Rssa")
	
	# check if all values are equal -> no seasonality return 0
	if (AllEqual(ts)) return(ts(rep(0, length(ts)), start=start(ts), freq=frequency(ts)))
	
	# fill in NA values with constant value
	pos.na <- (1:length(ts))[is.na(ts)]
	if (length(pos.na) > 0) ts[pos.na] <- approx(x=time(ts), y=ts, xout=time(ts)[pos.na], method="const", rule=c(2,2))$y
	
	# perform a singular spectrum analysis on the data
	ssa <- ssa(ts, kind="1d-ssa", L=as.integer(length(ts)*0.9))
	ssa.ts <- Rssa:::reconstruct(ssa, groups=as.list(1:nu(ssa)))
	# plot(ssa, type="values"); plot(ssa, type="series"); plot(ssa, type="paired")
	
	# function to check if SSA component is a seasonal component
	Is.seasonal <- function(s) {
		# estimate spectral density of ssa component
		spec <- spec.pgram(s, plot=FALSE)
		# get frequency at maximum spectrum value => periodic length of the time series
		spec.max <- which.max(spec$spec)
		freq <- spec$freq[spec.max]
		# calculate the wavelength in time units
		ssa.wavelength <- frequency(s) / freq
		ssa.wavelength[ssa.wavelength > length(s)] <- length(s)
		# check to which time series component group this wavelength belongs
		is.seasonal <- FALSE
		is.seasonal[(ssa.wavelength > frequency(s)-1) & (ssa.wavelength <= frequency(s)+1)] <- TRUE	# seasonal component if wavelength is 11, 12 or 13 months
		return(is.seasonal)
	}
	
	# identify if SSA component is a seasonal component
	na.b <- lapply(ssa.ts, FUN=function(x) { all(is.na(x)) })
	ssa.ts <- ssa.ts[((1:length(ssa.ts))[!unlist(na.b)])] # remove components which includes only NA values
	seasonal <- llply(ssa.ts, .fun=Is.seasonal)
	ssa.seasonal.ts <- ssa.ts[unlist(seasonal)]	
	
	# aggregate all seasonal SSA components to the total seasonal component
	if (length(ssa.seasonal.ts) == 0) {
		St_est <- ts(rep(0, length(ts)), start=start(ts), freq=frequency(ts)) 
	} else {
		St_est <- rowSums(as.data.frame(ssa.seasonal.ts))
		St_est <- ts(St_est, start=start(ts), freq=frequency(ts)) 
		St_est <- St_est - mean(St_est)
	}
	if (length(pos.na) > 0) St_est[pos.na] <- NA
	return(St_est)
}, ex=function() {
# load a time series of Normalized Difference Vegetation Index
data(ndvi)
plot(ndvi)

# estimate the seasonal cycle using SSA
ndvi.cycle <- SSASeasonalCycle(ndvi)
plot(ndvi.cycle)

# the mean seasonal cycle is centered to 0 - add the mean of the time series if you want to overlay it with the original data:
plot(ndvi)
lines(ndvi.cycle + mean(ndvi, na.rm=TRUE), col="blue")
	
})