IsPermanentGap <- structure(function(
	##title<< 
	## Identify if a gap is a permanent gap that occurs every year
	##description<<
	## 
	
	Yt, 
	### univariate time series of class \code{\link{ts}}
	
	min.gapfrac = 0.2,
	### How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.
				
	lower = TRUE,
	### identify lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)
 
	...
	### further arguments (currently not used)
	
	##references<< 
	## 
	
	##seealso<<
	## \code{\link{TsPP}}

) {

	if (class(Yt) != "ts") stop("Yt should be from class 'ts'.")
	
	# get time series properties
	freq <- frequency(Yt)
	n <- length(Yt)
	time <- time(Yt)
	start <- start(Yt)
	tsp <- tsp(Yt)
	nyears <- length(Yt) / freq	
	
	# identify position of time series values: lower or upper regions?
	if (!is.null(lower)) {
		# do initial gap filling using splines
		Yt2 <- c(Yt, Yt, Yt)
		Ytf <- na.approx(Yt2)
		Ytf <- ts(runmed(Ytf, 3)[(n+1):(n+1+n)], start=start, frequency=freq)
		
		# split each year in two halfs
		if (!AllEqual(Ytf)) {
			dc <- Decompose(Yt)
			Tt <- dc[, grep("Trend", colnames(dc))] + dc[, grep("IAV", colnames(dc))] + dc[, grep("Mean", colnames(dc))]
			
			# identifiy if gap is lower or upper gap
			if (lower) {
				Pt <- (Ytf < Tt)
			} else {
				Pt <- (Ytf > Tt)
			}			
		} else {
			Pt <- Yt
			Pt[] <- TRUE
		}

	} else {
		Pt <- Yt
		Pt[] <- TRUE
	}
	
	# identify NAs
	Na <- ts(as.integer(is.na(Yt)))	# NA values
	tsp(Na) <- tsp
		
	# identifiy how often an observation is NA (%) --> fraction of gaps
	Na.agg <- aggregate(as.vector(Na), list(as.vector(cycle(Na))), "sum", na.rm=TRUE)$x
	Gf <- Na.agg / nyears
	Gf <- ts(Gf[as.vector(cycle(Na))], start=start, freq=freq)
		
	# identify permanent (lower or upper) gaps
	Pg <- Gf > min.gapfrac & Pt
	
	# fraction of permanent gaps
	Pg.agg <- aggregate(as.vector(Pg), list(as.vector(cycle(Pg))), "sum", na.rm=TRUE)$x
	Pgf <- Pg.agg / nyears
	Pgf <- ts(Pgf[as.vector(cycle(Pg))], start=start, freq=freq)
	
	Pg <- (Pgf > 0.5)
	return(Pg)

}, ex=function() {
	# load NDVI data
	data(ndvi)

	# introduce some systematic gaps in january, february, december and july
	gaps <- ndvi
	gaps[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12 | cycle(ndvi) == 7] <- NA
	gaps[1] <- 0.2
	gaps[7] <- 0.3
	plot(gaps)
	
	# identifiy permanent winter gaps only
	IsPermanentGap(gaps, lower=TRUE)
	
	# identify permanent summer gaps
	IsPermanentGap(gaps, lower=FALSE)
	
	# identify all permanent gaps
	IsPermanentGap(gaps, lower=NULL)
})


FillPermanentGaps <- structure(function(
	##title<< 
	## Fills permanent winter gaps
	##description<<
	## Satellite time series are often affected by permanent gaps like missing observations during winter periods. Often time series methods can not deal with missing observations and require gap-free data. This function fills winter gaps with a constant fillval or according to the approach described in Beck et al. (2006).
	
	Yt, 
	### univariate time series of class \code{\link{ts}}
	
	min.gapfrac = 0.2,
	### How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.
	
	lower = TRUE,
	### fill lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)
	
	fillval = NA,
	### constant fill values for gaps. If NA the fill value will be estimated from the data using FUN. 
	
	fun = min,
	### function to be used to compute fill values. By default, minimum.
 
	...
	### further arguments (currently not used)
	
	##references<< 
	## 
	
	##seealso<<
	## \code{\link{TsPP}}

) {
	if (class(Yt) != "ts") stop("Yt should be from class 'ts'.")
	
	# get time series properties
	freq <- frequency(Yt)
	n <- length(Yt)
	time <- time(Yt)
	start <- start(Yt)
	tsp <- tsp(Yt)
	nyears <- length(Yt) / freq	
	
	# identify permanent gaps
	Pg <- IsPermanentGap(Yt, lower=lower, min.gapfrac=min.gapfrac)
	Pg.id <- (1:length(Pg))[Pg]
	
	i <- 1
	if (any(Pg)) {
		# estimate fill values
		i <- 1
		while (is.na(fillval) & i < 10) {
			x <- Yt[Pg.id]

			# fraction of NA values
			fna <- sum(is.na(x))/length(x)
			if (fna > 0.7) {
				fillval <- NA
			} else {
				fillval <- do.call(fun, list(x, na.rm=TRUE))
			}
			Pg.id <- unique(c(Pg.id, Pg.id + 1, Pg.id - 1))
			Pg.id <- Pg.id[Pg.id > 0]
			i <- i + 1
		}
		Yt[Pg] <- fillval
	}
	
	return(Yt)
	### The function returns a time series with filled permanent gaps.
}, ex=function() {

# load NDVI data
data(ndvi)
plot(ndvi)

# sample some winter months to be set as gaps
winter <- (1:length(ndvi))[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12]
gaps <- sample(winter, length(winter)*0.3)

# introduce systematic winter gaps in time series
ndvi2 <- ndvi
ndvi2[gaps] <- NA
plot(ndvi2)
IsPermanentGap(ndvi2)

# fill winter with observed minimum
fill <- FillPermanentGaps(ndvi2)	
plot(fill, col="red"); lines(ndvi)

# fill winter with mean
fill <- FillPermanentGaps(ndvi2, fun=mean)	
plot(fill, col="red"); lines(ndvi)

# fill winter with 0
fill <- FillPermanentGaps(ndvi2, fillval=0)	
plot(fill, col="red"); lines(ndvi)

})


