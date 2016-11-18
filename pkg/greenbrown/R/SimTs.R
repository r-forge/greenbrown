SimSeas <- structure(function(
	##title<< 
	## Simulate the seasonal component of a surrogate time series
	
	##description<<The function simulates the seasonal component of a time series based on a cosinus harmonic term.
	
	##references<<
	
	range, 
	### range of the seasonal cycle (seasonal amplitude)
	
	n=360, 
	### length of the time series
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.
		
	##seealso<<
	## \code{\link{SimTs}}
) {
	St <- ts(cos((1:n - (freq/2+1) ) / freq * 2 * pi) * (range / 2), start=start, frequency=freq)  
	return(St)
	### time series of class \code{\link[stats]{ts}}
}, ex=function(){
St <- SimSeas(range=0.6)
plot(St)
})


SimIAV <- structure(function(
	##title<< 
	## Simulate the inter-annual variability component of a surrogate time series
	
	##description<<The function simulates the inter-annual variability component of a time series based on normal-distributed random values.
	
	sd=0.015, 
	### standard deviation of the annual mean values
	
	range=sd*2,
	### range of the annual mean values
	
	nyears=30, 
	### number of years
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.
		
	##seealso<<
	## \code{\link{SimTs}}
) {
	iav <- rnorm(nyears, 0, sd)
	if ((max(iav) - min(iav)) > range) {
		iav[iav > (range/2)] <- 0
		iav[iav < (range/2 * -1)] <- 0
	}
	iav <- ts(iav, frequency=1)
	# try to avoid sub-trends in IAV by 'destroying' any order
	iav <- iav[order(iav)]
	o <- sample(seq(1, nyears))
	iav <- iav[o]
	iav <- iav - median(iav)
	iav <- ts(iav[rep(1:nyears, each=freq)], start=start, frequency=freq)
	return(iav)
	### time series of class \code{\link[stats]{ts}}
}, ex=function(){
It <- SimIAV(sd=0.015, range=0.05, nyears=30)
plot(It)
})


SimRem <- structure(function(
	##title<< 
	## Simulate the short-term variability component of a surrogate time series
	
	##description<<
	## The function simulates the short-term variability component (remainder component) of a time series (remainder component) based on normal-distributed random values.
	
	sd=0.05, 
	### standard deviation of short-term anomalies
	
	range=sd*3,
	### range of short-term anomalies
	
	n=360, 
	### length of the time series
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.
		
	##seealso<<
	## \code{\link{SimTs}}
) {
	rem <- rnorm(n, 0, sd)
	if ((max(rem) - min(rem)) > range) {
		rem[rem > (range/2)] <- 0
		rem[rem < (range/2 * -1)] <- 0
	}
	rem <- rem - median(rem)
	rem <- ts(rem, start=start, frequency=freq)
	return(rem)
	### time series of class \code{\link[stats]{ts}}
}, ex=function(){
Rt <- SimRem(sd=0.02, range=0.08)
plot(Rt)
})



SimTrend <- structure(function(
	##title<< 
	## Simulate trend and breakpoints of a surrogate time series
	
	##description<< The function simulates the trend component with breakpoints of a time series.
	
	slope=c(0.002, -0.004), 
	### slope of the trend in each time series segment. \code{slope} should be a numeric vector. The length of this vector determines the number of segments.
	
	breaks=165, 
	### position of the breakpoints in the time series. You should specify one more slope than breakpoint.
	
	abrupt=TRUE, 
	### Should the trend at the breakpoints change abrupt (\code{TRUE}) or gradual (\code{FALSE})?
	
	n=360, 
	### length of the time series
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.
		
	##seealso<<
	## \code{\link{SimTs}}
) {
	nseg <- length(breaks) + 1
	breakdates <- c(1, breaks, n)
	Tt <- NULL
	for (s in 1:nseg) {
		n.seg <- length(breakdates[s]:(breakdates[s+1]-1))
		Tt.seg <- (1:n.seg) * slope[s]
		Tt.seg <- Tt.seg - mean(Tt.seg)
		if (s == nseg) n.seg <- n.seg + 1
		Tt.before <- Tt[length(Tt)]
		if (is.null(Tt.before)) Tt.before <- 0 
		if (!abrupt & (s > 1)) { # link both trends in case of gradual change
			offset <- Tt.seg[1] - Tt.before
			Tt.seg <- Tt.seg - offset
		}
		Tt <- c(Tt, Tt.seg)
	}
	Tt[n] <- Tt[n - 1]
	Tt <- Tt - mean(range(Tt))	
	Tt <- ts(Tt, start=start, frequency=freq) 
	return(Tt)
	### time series of class \code{\link[stats]{ts}}
}, ex=function(){
Tt <- SimTrend(slope=c(0.003, -0.001), breaks=150)
plot(Tt)
})


SimTs <- structure(function(
	##title<< 
	## Simulate surrogate time series
	
	##description<<
	## The function simulates a surrogate (artificial) time series based on the defined properties. See Forkel et al. 2013 for a description how time series are simulated with this function.
	
	M=0.35, 
	### mean of the time series
	
	Tslope=c(0.002, -0.004), 
	### slope of the trend in each time series segment. \code{slope} should be a numeric vector. The length of this vector determines the number of segements.
	
	Isd=0.015, 
	### standard deviation of the annual mean values (inter-annual variability)
	
	Irange=0.03, 
	### range of the annual mean values (inter-annual variability)
	
	Srange=0.5, 
	### range of the seasonal cycle (seasonal amplitude)
	
	Rsd=0.05, 
	### standard deviation of short-term anomalies
		
	Rrange=0.1,	
	### range of short-term anomalies
	
	breaks=120, 
	### position of the breakpoints in the time series. You should specify one more slope than breakpoint.
	
	abrupt=TRUE,	
	### Should the trend at the breakpoints change abrupt (\code{TRUE}) or gradual (\code{FALSE})?	
	
	n=360, 
	### length of the time series
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link[stats]{ts}} for further examples.
	
	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.
		
	##seealso<<
	## \code{\link{SimTs}}
) {

	# get time series properties
	time <- ts(rep(NA, n), start=start, frequency=freq)
	time <- time(time)
	years <- as.integer(as.vector(time))
	nyears <- length(unique(years))

	# mean
	Mt <- ts(rep(M, n), start=start, frequency=freq)

	# seasonal
	St <- SimSeas(Srange, n=n, start=start, freq=freq)
	
	# IAV
	It <- SimIAV(Isd, Irange, nyears, start=start, freq=freq)
	
	# remainder
	Rt <- SimRem(Rsd, Rrange, n=n, start=start, freq=freq)
	
	# create trend (per segment)
	Tt <- SimTrend(Tslope, breaks, abrupt=abrupt, n=n, start=start, freq=freq)
	
	# plot(Tt)
	
	# create total time series
	Yt <- Mt + Tt + It + St + Rt
	
	sim.ts <- cbind(Yt=Yt, Mt=Mt, Tt=Tt, It=It, St=St, Rt=Rt)
	names(sim.ts) <- c("Yt", "Mt", "Tt", "It", "St", "Rt")
	return(sim.ts)
	### The function returns multiple time series of class \code{\link[stats]{ts}} with the following components: total time series, mean, trend component, inter-annual variability component, seasonal component, short-term component.
}, ex=function(){
# simulate artificial time series
x <- SimTs(M=0.4, Tslope=0.0008, Isd=0.015, Irange=0.03, Srange=0.5, Rsd=0.05, 
	Rrange=0.1, breaks=NULL, abrupt=TRUE, n=360, start=c(1982, 1), freq=12)
plot(x)

x <- SimTs(M=0.35, Tslope=c(0.002, -0.0015), Isd=0.015, Irange=0.03, Srange=0.5, 
   Rsd=0.05, Rrange=0.1, breaks=120, abrupt=TRUE, n=360, start=c(1982, 1), freq=12)
plot(x)

x <- SimTs(M=0.4, Tslope=c(0.003, -0.001, 0), Isd=0.03, Irange=0.08, Srange=0.3, 
   Rsd=0.06, 	Rrange=0.2, breaks=c(100, 210), abrupt=FALSE, 
   n=360, start=c(1982, 1), freq=12)
plot(x)

})

