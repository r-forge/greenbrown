MannKendallSeg <- structure(function(
	##title<< 
	## Apply MannKendall test for segments of a time series
	
	##description<<
	## The function call \code{\link{MannKendall}} for different segments of a time series
	
	Yt,
	### univariate time series of class \code{\link{ts}}
	
	seg = NULL,
	### a vector indicating segments of a time series. If NULL, provide bp
	
	bp = NoBP()
	### detected breakpoints in the time series as returned by \code{\link[strucchange]{breakpoints}}
	
	##references<<  
		
	##seealso<<
	## \code{\link{MannKendall}}
) {

  b <- !is.na(Yt)
   if (is.null(seg)) {
	   # has the time series breakpoints?
	   has.bp <- !is.na(bp$breakpoints[1])

	   # get segments according to breakpoints
	   if (has.bp) {
		   seg <- breakfactor(bp)
	   } else {
		   seg <- factor(rep(1, sum(b)))
	   }
	   seg <- as.numeric(seg)
   }
  d <- data.frame(time=time(Yt), response=Yt, seg=NA)
  d$seg[b] <- seg
  d <- na.omit(d)
	
	# get trend uncertainty for each segment
	result <- ldply(as.list(c(0, unique(seg))), function(s) {
	  if (s == 0) {
	    ti.seg <- d$time
	    x <- d$response
	  } else {
	    ti.seg <- d$time[d$seg == s]
	    x <- d$response[d$seg == s]
	  }
	   start <- ti.seg[1]
	   end <- ti.seg[length(ti.seg)]

	   # mann kendall test
	   mk <- MannKendall(x)
	   
	   # linear regression
	   m <- lm(x ~ ti.seg)
	   m.sum <- summary(m)
	   slope <- coef(m)[2]
	   slope.se <- m.sum$coefficients[2, grep("Error", colnames(m.sum$coefficients))]
	   pval <- m.sum$coefficients[2, grep("Pr", colnames(m.sum$coefficients))]
	   perc <- abs(slope) / abs(mean(x, na.rm=TRUE)) * 100
	   if (slope < 0) perc <- perc * -1
	
		result <- data.frame(seg=s, start=start, end=end, mk.tau=mk$tau, mk.pvalue=mk$sl, lm.slope=slope, lm.slope.se=slope.se, lm.slope.pvalue=pval, lm.slope.perc=perc)
		return(result)
	})
	
	return(result)
	### The function returns a data.frame with the estimated Mann-Kendall tau and p-value for each segment of the time series.
}, ex=function(){

# aggregate time series to annual time steps
ndvi <- aggregate(ndvi, FUN=mean)
plot(ndvi)

# MannKendall test for different segments
seg <- c(rep(1, length(start(ndvi)[1]:1995)), rep(2, length(1996:end(ndvi)[1])))
mk <- MannKendallSeg(ndvi, seg)
mk

# MannKendall test for segments that are defined by breakpoints
bp <- breakpoints(ndvi ~ time(ndvi))
mk <- MannKendallSeg(ndvi, bp=bp)
mk

# MannKendall test without segments
MannKendallSeg(ndvi)

})
