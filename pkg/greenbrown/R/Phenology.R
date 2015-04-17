Phenology <- structure(function(
	##title<< 
	## Calculate phenology metrics in time series
	##description<<
	## This function calculates from time series annual metrics of vegetation phenology:
	## \itemize{ 
	## \item{ \code{sos} start of season }
	## \item{ \code{eos} end of season }
	## \item{ \code{los} length of season }
	## \item{ \code{pop} position of peak value (maximum) }
	## \item{ \code{pot} position of trough value (minimum) }
	## \item{ \code{mgs} mean growing season value }
	## \item{ \code{peak} peak value (maximum) }
	## \item{ \code{trough} trough value (minimum) }
	## \item{ \code{msp} mean spring value }
	## \item{ \code{mau} mean autumn value }
	## \item{ \code{rsp} rate of spring greenup (not all methods) }
	## \item{ \code{rau} rate of autumn senescence rates (not all methods) }
	## }
	## The calculation of these metrics is performed in three steps and by using different methods:
	## \itemize{ 
	## \item{ Step 1: Filling of permanent (winter) gaps. See \code{\link{FillPermanentGaps}}}
	## \item{ Step 2: Time series smoothing and interpolation. See \code{\link{TsPP}} }	
	## \item{ Step 3: Detection of phenology metrics. Phenology metrics are estimated from the gap filled, smoothed and interpolated time series. This can be done by treshold methods (\code{\link{PhenoTrs}}) or by using the derivative of the time series (\code{\link{PhenoDeriv}}). }
	## }
	
	Yt, 
	### univariate time series of class \code{\link{ts}}
	
	approach = c("White", "Trs", "Deriv"), 
	### Approach to be used to calculate phenology metrics from smoothed time series. 'White' by sclaing annual cycles between 0 and 1 (White et al. 1997, see \code{\link{PhenoTrs}}); 'Trs' for simple tresholds (\code{\link{PhenoTrs}}); 'Deriv' by using the derivative of the smoothed function (\code{\link{PhenoDeriv}}).
	
	min.mean = 0.1,
	### minimum mean annual value in order to calculate phenology metrics. Use this threshold to suppress the calculation of metrics in grid cells with low average values	
	
	trs = NULL,
	### threshold to be used to determine SOS and EOS if method 'Trs' is used. If method 'Trs' is used but trs is NULL than trs will be computed from the long-term mean of Yt.
	
	fpg = FillPermanentGaps,
	### Filling of permanent gaps: If NULL, permanent gaps will be not filled, else the function \code{\link{FillPermanentGaps}} will be applied.
	
	tsgf = "TSGFspline",
	### Temporal smoothing and gap filling: Function to be used for temporal smoothing, gap filling and interpolation of the time series. If NULL, this step will be not applied. Otherwise a function needs to be specified. Exisiting functions that can be applied are \code{\link{TSGFspline}}, \code{\link{TSGFlinear}}, \code{\link{TSGFssa}}, \code{\link{TSGFdoublelog}}  
	
	interpolate = TRUE,
	### Should the smoothed and gap filled time series be interpolated to daily values?
	
	min.gapfrac = 0.2,
	### How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.	
	
	lower = TRUE,
	### For filling of permanent gaps: fill lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)
	
	fillval = NA,
	### For filling of permanent gaps: constant fill values for gaps. If NA the fill value will be estimated from the data using fun. 
	
	fun = min,
	### For filling of permanent gaps: function to be used to compute fill values. By default, minimum.
	
	method = c("Elmore", "Beck"),
	### If 'tsgf' is TSGFdoublelog: Which kind of double logistic curve should be used to smooth the data? 'Elmore' (Elmore et al. 2012, see \code{\link{FitDoubleLogElmore}}) or 'Beck' (Beck et al. 2006, see \code{\link{FitDoubleLogBeck}}) .	
	
	backup = NULL,
	### Which backup algorithm should be used instead of TSGFdoublelog for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"	
	
	check.seasonality = 1:3,
	### Which methods in \code{\link{Seasonality}} should indicate TRUE (i.e. time series has seasonality) in order to calculate phenology metrics? 1:3 = all methods should indicate seasonality, Set to NULL in order to not perform seasonality checks.
	
	...
	### further arguments (currently not used)
	
	##details<<
	## This function allows to calculate phenology metrics on time series. This method  can be applied to gridded (raster) data using the function \code{\link{PhenologyRaster}}.
	
	##references<< 
	## Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334. \cr
	## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674. \cr	
	## White M.A., P.E. Thornton and S.W. Running (1997): A continental phenology model for monitoring vegetation responses to interannual climatic variability. - Global Biogeochemical Cycles 11, 217-234. 
	
	##seealso<<
	## \code{\link{PhenologyRaster}}, \code{\link{TSGFspline}}, \code{\link{TSGFssa}}, \code{\link{TSGFdoublelog}}, \code{\link{FitDoubleLogElmore}}, \code{\link{FitDoubleLogBeck}}

) {

	if (class(Yt) != "ts") stop("Yt should be from class 'ts'.")
	start <- start(Yt)
	freq <- frequency(Yt)
	nout <- 12 # number of output variables
	if (length(approach) > 1) approach <- approach[1]
	
	# time series pre-processing
	Yt1 <- TsPP(Yt, fpg=fpg, tsgf=tsgf, interpolate=interpolate, min.gapfrac=min.gapfrac, lower=lower, fillval=fillval, fun=fun, method=method, backup=backup)
	
	# check if the time series has Seasonality
	calc.pheno <- FALSE
	seasonal <- rep(FALSE, 3)
	if (!AllEqual(Yt1)) {
		if (is.null(check.seasonality)) {
			calc.pheno <- TRUE
		} else {
			seasonal <- Seasonality(Yt1)
			if (all(seasonal[check.seasonality])) calc.pheno <- TRUE # calculate phenology metrics only if 3 methods indicate seasonality
		}
	}
	
	# estimation of phenology metrics
	if (approach == "Trs") {	
		# treshold method based on mean treshold
		if (is.null(trs)) trs <- mean(Yt1, na.rm=TRUE)
		ts.agg <- aggregate(Yt1, FUN=PhenoTrs, approach="Trs", trs=trs, min.mean=min.mean, calc.pheno=calc.pheno)	
		
	} else if (approach == "White") {	
		# White et al. 1997
		ts.agg <- aggregate(Yt1, FUN=PhenoTrs, approach="White", trs=trs, min.mean=min.mean, calc.pheno=calc.pheno)	

	} else if (approach == "Deriv") {	
		# derivative method
		ts.agg <- aggregate(Yt1, FUN=PhenoDeriv, min.mean=min.mean, calc.pheno=calc.pheno)
	} else {
		stop(paste("Phenology: approach =", approach, "does not exist. Use 'Trs', 'White' or 'Deriv'."))
	}
		
	# reconstruct time series of metrics
	sos.ts <- ts(ts.agg[seq(1, length(ts.agg), by=nout)], start=start[1], frequency=1)
	eos.ts <- ts(ts.agg[seq(2, length(ts.agg), by=nout)], start=start[1], frequency=1)
	los.ts <- ts(ts.agg[seq(3, length(ts.agg), by=nout)], start=start[1], frequency=1)
	pop.ts <- ts(ts.agg[seq(4, length(ts.agg), by=nout)], start=start[1], frequency=1)
	pot.ts <- ts(ts.agg[seq(5, length(ts.agg), by=nout)], start=start[1], frequency=1)
	mgs.ts <- ts(ts.agg[seq(6, length(ts.agg), by=nout)], start=start[1], frequency=1)
	rsp.ts <- ts(ts.agg[seq(7, length(ts.agg), by=nout)], start=start[1], frequency=1)
	rau.ts <- ts(ts.agg[seq(8, length(ts.agg), by=nout)], start=start[1], frequency=1)
	peak.ts <- ts(ts.agg[seq(9, length(ts.agg), by=nout)], start=start[1], frequency=1)
	trough.ts <- ts(ts.agg[seq(10, length(ts.agg), by=nout)], start=start[1], frequency=1)
	msp.ts <- ts(ts.agg[seq(11, length(ts.agg), by=nout)], start=start[1], frequency=1)
	mau.ts <- ts(ts.agg[seq(12, length(ts.agg), by=nout)], start=start[1], frequency=1)
	
	# correct SOS, EOS and POP if they 'jump' over end of year and exclude outliers
	.correctDOY <- function(doy) {
		id <- 1:length(doy)
		atstart <- na.omit(id[doy <= 60]) # check if DOY is within first two monthy
		atend <- na.omit(id[doy >= 305]) # check if DOY is within last two months
		if (length(atstart) > 0 & length(atend) > 0) { # correct if some DOYs are at the end and some at the start of the year
			if (length(atstart) >= length(atend)) {
				doy[atend] <- doy[atend] - 365
			} else {
				doy[atstart] <- doy[atstart] + 365
			}
		}
		
		# exclude outliers if more than 60 days before or after median DOY
		med <- median(doy, na.rm=TRUE)
		rge <- IQR(doy, na.rm=TRUE) * 2
		doy[doy < (med - rge) | doy > (med + rge)] <- NA
		return(doy)
	}
	sos.ts <- .correctDOY(sos.ts)
	eos.ts <- .correctDOY(eos.ts)
	pop.ts <- .correctDOY(pop.ts)
	pot.ts <- .correctDOY(pot.ts)
	msp.ts[is.na(sos.ts)] <- NA
	mau.ts[is.na(eos.ts)] <- NA
	mgs.ts[is.na(sos.ts) & is.na(eos.ts)] <- NA
	los.ts[is.na(sos.ts) & is.na(eos.ts)] <- NA
	
	# prepare result
	phen <- list(
		series = Yt1,
		seasonal = seasonal,
		calc.pheno = calc.pheno,
		sos=sos.ts, 
		eos=eos.ts, 
		los=los.ts, 
		pop=pop.ts,
		pot=pot.ts,
		mgs=mgs.ts,
		peak=peak.ts,
		trough=trough.ts,
		msp=msp.ts,
		mau=mau.ts,
		rsp=rsp.ts,
		rau=rau.ts,
		freq.orig=freq,
		approach = approach,
		tsgf = tsgf
	)
	if (!is.null(tsgf)) {
		if (tsgf == "TSGFdoublelog") phen$method <- method[1]
	}
	class(phen) <- "Phenology"
	return(phen)
	### The function returns a "Phenology" object with the following components
	### \itemize{ 
	### \item{ \code{method} Selected method. }
	### \item{ \code{series} gap-filled, smoothed and daily interpolated time series }
	### \item{ \code{sos} annual time series of start of season }
	### \item{ \code{eos} annual time series of end of season }
	### \item{ \code{los} annual time series of length of season }
	### \item{ \code{pop} annual time series of position of peak (maximum) }
	### \item{ \code{pot} annual time series of position of trough (minimum) }
	### \item{ \code{mgs} annual time series of mean growing season values }
	### \item{ \code{peak} annual time series of peak value }
	### \item{ \code{trough} annual time series of trough value }
	### \item{ \code{msp} annual time series of mean spring value }
	### \item{ \code{mau} annual time series of mean autumn value }
	### \item{ \code{rsp} annual time series of spring greenup rates (only for methods 'Deriv' and 'Logistic')}
	### \item{ \code{rau} annual time series of autumn senescence rates (only for methods 'Deriv' and 'Logistic')}
	### }
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# introduce some missing values
winter <- (1:length(ndvi))[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12]
ndvi[sample(winter, length(winter)*0.5)] <- NA
plot(ndvi)

# spline fit and threshold
spl.trs <- Phenology(ndvi, tsgf="TSGFspline", approach="White") 
spl.trs
plot(spl.trs)	# default plot: start of season, end of season, position of peak
plot(spl.trs, type=c("los")) # length of season
plot(spl.trs, type=c("msp", "mgs", "mau", "peak")) # plot mean spring, growing season, autumn and peak values
plot(spl.trs$series, col="red"); lines(ndvi) # gap-filled and smoothed time series that was used to estimate phenology metrics

# calculate phenology metrics using different smoothing methods and approaches
lin.trs <- Phenology(ndvi, tsgf="TSGFlinear", approach="White") # linear interpolation/running median + treshold
lin.deriv <- Phenology(ndvi, tsgf="TSGFlinear", approach="Deriv") # linear interpolation/running median + derivativw
spl.trs <- Phenology(ndvi, tsgf="TSGFspline", approach="White") # spline + treshold
spl.deriv <- Phenology(ndvi, tsgf="TSGFspline", approach="Deriv") # spline + derivative
ssa.trs <- Phenology(ndvi, tsgf="TSGFssa", approach="White") # singular spectrum analysis + treshold
ssa.deriv <- Phenology(ndvi, tsgf="TSGFssa", approach="Deriv") # singular spectrum analysis + derivativw
beck.trs <- Phenology(ndvi, tsgf="TSGFdoublelog", method="Beck", approach="White") # double logistic fit + treshold
beck.deriv <- Phenology(ndvi, tsgf="TSGFdoublelog", method="Beck", approach="Deriv") # double logistic fit + derivative
elmore.trs <- Phenology(ndvi, tsgf="TSGFdoublelog", method="Elmore", approach="White") # double logistic fit + treshold
elmore.deriv <- Phenology(ndvi, tsgf="TSGFdoublelog", method="Elmore", approach="Deriv") # double logistic fit + derivative

# compare results: SOS and EOS
library(RColorBrewer)
type <- c("sos", "eos")
cols <- brewer.pal(10, "Paired")
nms <- c("Lin+Trs", "Lin+Deriv", "Spline+Trs", "Spline+Deriv", "SSA+Trs", "SSA+Deriv", "DoubleLog1+Trs", "DoubleLog1+Deriv", "DoubleLog2+Trs", "DoubleLog2+Deriv")
plot(lin.trs, col=cols[1], type=type, ylim=c(1, 365))
plot(lin.deriv, col=cols[2], type=type, add=TRUE)
plot(spl.trs, col=cols[3], type=type, add=TRUE)
plot(spl.deriv, col=cols[4], type=type, add=TRUE)
plot(ssa.trs, col=cols[5], type=type, add=TRUE)
plot(ssa.deriv, col=cols[6], type=type, add=TRUE)
plot(beck.trs, col=cols[7], type=type, add=TRUE)
plot(beck.deriv, col=cols[8], type=type, add=TRUE)
plot(elmore.trs, col=cols[9], type=type, add=TRUE)
plot(elmore.deriv, col=cols[10], type=type, add=TRUE)
legend("center", nms, text.col=cols, ncol=3, bty="n")

cor(cbind(lin.trs$sos, lin.deriv$sos, spl.trs$sos, spl.deriv$sos, ssa.trs$sos, ssa.deriv$sos, beck.trs$sos, beck.deriv$sos, elmore.trs$sos, elmore.deriv$sos), use="pairwise.complete.obs")
cor(cbind(lin.trs$eos, lin.deriv$eos, spl.trs$eos, spl.deriv$eos, ssa.trs$eos, ssa.deriv$eos, beck.trs$eos, beck.deriv$eos, elmore.trs$eos, elmore.deriv$eos), use="pairwise.complete.obs")

# compare results: LOS
type <- c("los")
plot(lin.trs, col=cols[1], type=type, ylim=c(130, 365))
plot(lin.deriv, col=cols[2], type=type, add=TRUE)
plot(spl.trs, col=cols[3], type=type, add=TRUE)
plot(spl.deriv, col=cols[4], type=type, add=TRUE)
plot(ssa.trs, col=cols[5], type=type, add=TRUE)
plot(ssa.deriv, col=cols[6], type=type, add=TRUE)
plot(beck.trs, col=cols[7], type=type, add=TRUE)
plot(beck.deriv, col=cols[8], type=type, add=TRUE)
plot(elmore.trs, col=cols[9], type=type, add=TRUE)
plot(elmore.deriv, col=cols[10], type=type, add=TRUE)
legend("bottom", nms, text.col=cols, ncol=5, bty="n")

# compare results: MSP, PEAK, TROUGH
type <- c("msp", "peak", "trough")
plot(lin.trs, col=cols[1], type=type, ylim=c(0.17, 0.37))
plot(lin.deriv, col=cols[2], type=type, add=TRUE)
plot(spl.trs, col=cols[3], type=type, add=TRUE)
plot(spl.deriv, col=cols[4], type=type, add=TRUE)
plot(ssa.trs, col=cols[5], type=type, add=TRUE)
plot(ssa.deriv, col=cols[6], type=type, add=TRUE)
plot(beck.trs, col=cols[7], type=type, add=TRUE)
plot(beck.deriv, col=cols[8], type=type, add=TRUE)
plot(elmore.trs, col=cols[9], type=type, add=TRUE)
plot(elmore.deriv, col=cols[10], type=type, add=TRUE)
legend("bottom", nms, text.col=cols, ncol=5, bty="n")


})





