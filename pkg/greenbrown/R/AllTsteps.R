AllTsteps <- structure(function(
	##title<< 
	## Convert an irregular zoo time series to a zoo time series with all time steps
	##description<<
	## Irregular 'zoo' time series with missing time steps are converted to a 'zoo' time series including all time steps. Observations at time steps that were missing in the original time series are filled with NA.

	x,
	### a time series of class 'zoo'
	
	by = "day",
	### time step of full time series, e.g. "day", "month"
	
	start.jan = FALSE,
	### Should the full time series start in January? If FALSE, the full time series will start at the first observation in x.
	
	end.dec = FALSE,
	### Should the full time series end in December? If FALSE, the full time series will end at the last observation in x.
	
	exclude.feb29 = FALSE,
	### Should 29th Februaries be excluded in case of a daily time series (i.e. if by = 'day')?
	
	...
	### further arguments (currently not used)
			
	##details<<
	## 
	
	##seealso<< 
	## 
) { 	
   nms <- names(x)
   ti <- time(x)
   yr <- format(ti, "%Y")
   day <- format(ti, "%d")
   start <- ti[1]
   end <- ti[length(ti)]
   if (by == "day" & start.jan) start <- as.Date(paste0(yr[1], "-01-01"))
   if (by != "day" & start.jan) start <- as.Date(paste0(yr[1], "-01-", day[1]))
   if (by == "day" & end.dec) end <- as.Date(paste0(yr[length(yr)], "-12-31"))
   if (by != "day" & end.dec) end <- as.Date(paste0(yr[length(yr)], "-12-", day[length(day)]))
   ti.full <- seq(start, end, by=by)
   x <- merge(x, zoo(NA, ti.full))
   
   if (by == "day" & exclude.feb29) {
      md <- format(ti.full, "%m-%d")
      bool <- "02-29" == md
      x <- x[!bool,]
      if (any(bool)) ti.full <- ti.full[!bool]
   }
   
   x <- x[, -ncol(x)]
   names(x) <- nms
   return(x)
}, ex=function() {
   x <- zoo(rnorm(5), as.Date(c("2010-01-15", "2010-02-15", "2010-07-15", "2010-08-15", "2010-09-15")))
   x
   AllTsteps(x, by="month")
})
