Seasonality <- structure(function(
	##title<< 
	## Check a time series for seasonality
	##description<<
	## This function checks a time series for seasonality using three different approaches: 
	## \itemize{ 
	## \item{ \code{'pgram'} computes a periodogram using fast fourier transformation (\code{\link{spec.pgram}}) and checks at which frequency the periodogram has a maximum. A maximum at a frequency of 1 indicates seasonality and the function returns TRUE. }
	## \item{ \code{'acf'} computes the auto-correlation function of the de-trended time series using \code{\link{acf}}. A minimum acf value at a lag of 0.5 indicates seasonality and the function returns TRUE. }
	## \item{ \code{'lm'} fits two linear models to the time series. The first model includes the trend and the seasonal cycle as factorial variable. The second model includes only the trend. Based on the \code{\link{BIC}} the better model is selected and the function returns TRUE if the first model (including a seasonal term) is better. }
	## }
	
	Yt, 
	### univariate time series of class \code{\link{ts}}.
	
	return.freq = FALSE,
	### if return.freq is TRUE the function returns the frequency at the maximum of the periodogram.
	
	plot = FALSE,
	### plot periodogram and acf? (see \code{\link{spec.pgram}} and \code{\link{acf}})
	
	...
	### further arguments (currently not used)
	
	##seealso<<
	## \code{\link{spec.pgram}}, \code{\link{acf}}, \code{\link{lm}}, \code{\link{BIC}}

) {
	# fill NA values
	Yt <- na.approx(Yt)
	
	# make three tests for seasoanlity: acf, periodogram and seasonal model
	seasonal <- rep(FALSE, 3)
	if (plot) {
	   op <- par()
	   par(mfrow=c(3,1))
	}
	
	# Test 1: periodogram
		# estimate spectral density 
		pgram <- spec.pgram(Yt, plot=plot, main="")
		if (plot) mtext("Periodogram", line=0.5, font=2)
		spec <- pgram$spec
		freq <- pgram$freq
		
		# select higher frequencies
		bool <- freq > 0.5 
		spec <- spec[bool]
		freq <- freq[bool]
		freq <- freq[which.max(spec)]
		if (return.freq) return(freq)
		if (freq > 0.95 & freq < 1.05) seasonal[1] <- TRUE
		
	# Test 2: auto-correlation function
		Tt <- tryCatch({
			stl(Yt, s.window="per")$time.series[,2]
		}, error = function(e) {
			NULL
		}, finally = function(x) {
			NULL
		})
		if (!is.null(Tt)) {
			At <- Yt - Tt	# detrend time series
			acf <- acf(At, plot=plot, main="", lag.max=length(At)*0.5)
			lag <- acf$lag[which.min(acf$acf)]
			if (plot) mtext("Auto-correlation function", line=0.5, font=2)
			if (lag > 0.4 & lag < 0.6) seasonal[2] <- TRUE
		} else {
			seasonal[2] <- FALSE
		}
		
	# Test 3: seasonal model
		df <- data.frame(Yt, seas=cycle(Yt), trend=time(Yt))
		df$seas <- factor(df$seas)
		m1 <- lm(Yt ~ seas + trend, data=df)
		m2 <- lm(Yt ~ trend, data=df)
		bic <- BIC(m1, m2)
		bic.min <- which.min(bic[,2])
		if (bic.min == 1) seasonal[3] <- TRUE
		if (plot) {
			plot(Yt ~ cycle(Yt), main="")
			if (plot) mtext("Season-trend model", line=0.5, font=2)
			if (bic.min == 1) points(fitted(m1) ~ cycle(Yt), col="blue")
			if (bic.min == 2) points(fitted(m2) ~ cycle(Yt), col="blue")
			legend("bottom", paste(c("seas+trend", "seas*trend", "trend")[bic.min], signif(bic$BIC)[bic.min], sep=": "), title="BIC of lm(Yt ~ x) with x:", bg="white", ncol=1, text.col="blue")
		}
		
	# return result
	names(seasonal) <- c("pgram", "acf", "lm")
	if (plot) on.exit(par(op))
	return(seasonal)
	### The function returns a boolean vector of length 3 including TRUE if a method detected seasonality or FALSE if a method did not detect seasonality. 
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)

# time series with strong Seasonality:
Yt <- SimTs(Srange = 0.2, Tslope=c(0.0004, 0))[,1]
plot(Yt)
Seasonality(Yt)

# time series with Seasonality and some noise
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.18, Rrange=0.25)[,1]
plot(Yt)
Seasonality(Yt)

# time series with Seasonality but many noise
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.22, Rrange=0.4)[,1]
plot(Yt)
Seasonality(Yt)

# time series without Seasonality 
Yt <- SimTs(Srange = 0.01, Tslope=c(0.0004, 0), Rsd=0.2, Rrange=0.4)[,1]
plot(Yt)
Seasonality(Yt)

# plot results for each seasonality method
Yt <- SimTs(Srange = 0.1, Tslope=c(0.0004, 0), Rsd=0.18, Rrange=0.25)[,1]
Seasonality(Yt, plot=TRUE)


})



