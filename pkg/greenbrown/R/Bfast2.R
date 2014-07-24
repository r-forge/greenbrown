Bfast2 <- structure(function(
	##title<< 
	## Break Detection in the Seasonal and Trend Component of a Univariate Time Series
		
	##description<<
	## The Bfast2 function is an alternative implementation of the bfast (breaks for additive seasonal and trend) algorithm. Bfast detects iteratively breaks  in seasonal and trend component of a time series. Bfast combines the iterative decomposition of time series into trend, seasonal and remainder components with significant break detection in the decomposed components of the time series. 
	## The Bfast2 function is based on the original implementation of \code{\link{bfast}} as in the bfast package. In difference to the original bfast implementation, Bfast2 allows missing values (NA) in the time series and thus does not require gap filling before the application of bfast.
	
	Yt,
	### univariate time series to be analyzed. This should be an object of class "ts" with a frequency greater than one. 
 
	h = 0.15, 
	### minimal segment size between potentially detected breaks in the trend model given as fraction relative to the sample size (i.e. the minimal number of observations in each segment divided by the total length of the timeseries.
	
    max.iter = 2, 
	### maximum amount of iterations allowed for estimation of breakpoints in seasonal and trend component. 
	
	breaks = 1, 
	### integer specifying the maximal number of breaks to be calculated. By default the maximal number allowed by h is used. 
	
	hpc = "none"
	###  A character specifying the high performance computing support. Default is "none", can be set to "foreach". Install the "foreach" package for hpc support. 
 
	
	##references<<
	## Verbesselt, J.; Hyndman, R.; Zeileis, A.; Culvenor, D., Phenological change detection while accounting for abrupt and gradual trends in satellite image time series. Remote Sensing of Environment 2010, 114, 2970-2980. \cr
 
	##seealso<<
	## \code{\link{bfast}}, \code{\link{plot.bfast}}
	
	) 
{
    ti <- time(Yt)
    f <- frequency(Yt)
    if (class(Yt) != "ts") 
        stop("Not a time series object")
    output <- list()
	
	# estimate initial seasonal model based on non-parametric STL
	isna <- is.na(Yt)
	Yt2 <- Yt
	Yt2[isna] <- mean(Yt, na.rm=TRUE)
    St <- stl(Yt2, "periodic")$time.series[, "seasonal"]
	St[isna] <- NA
    Tt <- 0

	# harmonic seasonal model
    w <- 1/f
    tl <- 1:length(Yt)
    co <- cos(2 * pi * tl * w)
    si <- sin(2 * pi * tl * w)
    co2 <- cos(2 * pi * tl * w * 2)
    si2 <- sin(2 * pi * tl * w * 2)
    co3 <- cos(2 * pi * tl * w * 3)
    si3 <- sin(2 * pi * tl * w * 3)
    smod <- Wt ~ co + si + co2 + si2 + co3 + si3
	
	# prepare data for analysis
    data <- na.omit(data.frame(Yt, St, ti, tl, co, si, co2, si2, co3, si3))
   
    Vt.bp <- 0
    Wt.bp <- 0
    CheckTimeTt <- 1
    CheckTimeSt <- 1
    i <- 0
    while ((!identical(CheckTimeTt, Vt.bp) | !identical(CheckTimeSt, Wt.bp)) & i < max.iter) {
        CheckTimeTt <- Vt.bp
        CheckTimeSt <- Wt.bp
        Vt <- data$Yt - data$St
		data$Vt <- Vt
		data <- na.omit(data)
		
		# estimate trend breaks
        bp.Vt <- breakpoints(Vt ~ ti, h = h, breaks = breaks, hpc = hpc, data=data)
        nobp.Vt <- is.na(breakpoints(bp.Vt)[1])
        if (nobp.Vt) {
            fm0 <- lm(Vt ~ ti, data=data)
            Vt.bp <- 0
            data$Tt <- ts(fitted(fm0))
            ci.Vt <- NA
        } else {
			data$bp.Vt <- breakfactor(bp.Vt)
            fm1 <- lm(Vt ~ bp.Vt/ti, data=data)
            ci.Vt <- confint(bp.Vt, het.err = FALSE)
            Vt.bp <- ci.Vt$confint[, 2]
            data$Tt <- ts(fitted(fm1))
        }
        
		# estimate seasonal breaks
        data$Wt <- data$Yt - data$Tt
        bp.Wt <- breakpoints(smod, h = h, breaks = breaks, hpc = hpc, data=data)
        nobp.Wt <- is.na(breakpoints(bp.Wt)[1])
        if (nobp.Wt) {
            sm0 <- lm(smod, data=data)
            data$St <- ts(fitted(sm0))
            Wt.bp <- 0
            ci.Wt <- NA
        } else {
            sm1 <- lm(Wt ~ (co + si + co2 + si2 + co3 + si3) %in% breakfactor(bp.Wt), data=data)
            data$St <- ts(fitted(sm1))
            ci.Wt <- confint(bp.Wt, het.err = FALSE)
            Wt.bp <- ci.Wt$confint[, 2]
        }
		data$Nt <- data$Yt - data$Tt - data$St
		
		# prepare output data
		data2 <- data.frame(matrix(NA, length(ti), ncol(data)))
		colnames(data2) <- colnames(data)
		data2$ti <- ti
		data2[match(data$ti, data2$ti), ] <- data

        i <- i + 1
		Tt <- ts(data2$Tt)
		tsp(Tt) <- tsp(Yt)
		St <- ts(data2$St)
		tsp(St) <- tsp(Yt)
		Nt <- ts(data2$Nt)
		tsp(Nt) <- tsp(Yt)		
		Vt <- ts(data2$Vt)
		tsp(Vt) <- tsp(Yt)		
		Wt <- ts(data2$Wt)
		tsp(Wt) <- tsp(Yt)				
        output[[i]] <- list(Tt = Tt, St = St, Nt = Nt, Vt = Vt, bp.Vt = bp.Vt, Vt.bp = Vt.bp, ci.Vt = ci.Vt, 
            Wt = Wt, bp.Wt = bp.Wt, Wt.bp = Wt.bp, ci.Wt = ci.Wt)
    }
    if (!nobp.Vt) {
        Vt.nrbp <- length(bp.Vt$breakpoints)
        co <- coef(fm1)
        Mag <- matrix(NA, Vt.nrbp, 3)
        for (r in 1:Vt.nrbp) {
            if (r == 1) 
                y1 <- co[1] + co[r + Vt.nrbp + 1] * ti[Vt.bp[r]]
            else y1 <- co[1] + co[r] + co[r + Vt.nrbp + 1] * 
                ti[Vt.bp[r]]
            y2 <- (co[1] + co[r + 1]) + co[r + Vt.nrbp + 2] * 
                ti[Vt.bp[r] + 1]
            Mag[r, 1] <- y1
            Mag[r, 2] <- y2
            Mag[r, 3] <- y2 - y1
        }
        index <- which.max(abs(Mag[, 3]))
        m.x <- rep(Vt.bp[index], 2)
        m.y <- c(Mag[index, 1], Mag[index, 2])
        Magnitude <- Mag[index, 3]
        Time <- Vt.bp[index]
    }
    else {
        m.x <- NA
        m.y <- NA
        Magnitude <- 0
        Time <- NA
        Mag <- 0
    }
    return(structure(list(Yt = Yt, output = output, nobp = list(Vt = nobp.Vt, 
        Wt = nobp.Wt), Magnitude = Magnitude, Mags = Mag, Time = Time, 
        jump = list(x = ti[m.x], y = m.y)), class = "bfast"))
	### The function returns an object of the class "bfast", see \code{\link{bfast}} for details.
}, ex=function() {
# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)
	
# set some missing values in the data
ndvi[c(60:80, 245:246, 255)] <- NA
ndvi[ndvi < 0.22] <- NA
plot(ndvi)

# the original bfast algorithm does not work with gaps:
# bf <- bfast(ndvi, season="harmonic")

# the Bfast2 implementation can handle gaps:
bf <- Bfast2(ndvi, breaks=1)
plot(bf, type="all")
	
})	



