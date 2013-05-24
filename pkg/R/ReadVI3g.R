ReadVI3g <- structure(function(
	##title<<
	## Read and pre-process GIMMS VI3g binary files
	
	##description<< This function reads GIMMS VI3g binary files, pre-processes the values (exclusion of flagged values, subset for area of interest) and returns the result as a raster layer. The function can be used to read the original GIMMS NDVI3g data files to R.
	
	file,
	### GIMMS VI3g file name

	flag=2:7,
	### vector of quality flags that should be excluded. Default: 2:7 (all values with reduced quality excluded). If you want to keep all values set \code{flag=NA}.
	
	ext=c(-180.0, 180.0, -90.0, 90.0)
	### extent (xmin, xmax, ymin, ymax) for which to extract the data
	
	##details<< The GIMMS NDVI3g dataset comes with the following quality flags:	
	## \itemize{ 
	## \item{ FLAG = 1 (Good value) }
	## \item{ FLAG = 2 (Good value, possibly snow) }
	## \item{ FLAG = 3 (NDVI retrieved from spline interpolation) }
	## \item{ FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow) }
	## \item{ FLAG = 5 (NDVI retrieved from average seasonal profile)}
	## \item{ FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow) }
	## \item{ FLAG = 7 (missing data) }
	## }	
) {

	# settings fro GIMMS VI3g files
	ncol <- 4320	# number of pixels along longitudes
	nrow <- 2160	# number of pixels along latitude
	xmin <- -180.0+1/24
	xmax <- 180.0-1/24
	ymin <- -90.0+1/24
	ymax <- 90.0-1/24
	ll <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
	
	# read data
	data.v <- readBin(file, "integer", n=10000000, size=2, signed=TRUE, endian="big")
	
	# replace missing and water values with NA
	data.v[data.v == -10000] <- NA
	data.v[data.v == -5000] <- NA	

	# get flags and data from data stream
	flag.v <- data.v - floor(data.v / 10) * 10 + 1
	data.v <- floor(data.v / 10) / 1000	

	# mask flagged values
	# to retrieve the original ndvi  and flagW values
 	# flagW = ndvi3g-floor(ndvi3g/10)*10 + 1;
 	# ndvi = floor(ndvi3g/10)/1000
    # The meaning of the FLAG:
	# FLAG = 7 (missing data)
	# FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow)
	# FLAG = 5 (NDVI retrieved from average seasonal profile)
	# FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow)
	# FLAG = 3 (NDVI retrieved from spline interpolation)
	# FLAG = 2 (Good value, possibly snow)
	# FLAG = 1 (Good value)		
	
	if (!all(is.na(flag))) {
		for (i in 1:length(flag)) data.v[flag.v == flag[i]] <- NA
	}
		
	# convert data vector to matrix
	data.m <- matrix(data.v, ncol=ncol, nrow=nrow)

	# convert matrix to raster layer
	data.r <- raster(data.m, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs=ll)
	
	# crop to extent
	if (ext[1] > xmin | ext[2] < xmax | ext[3] > ymin | ext[4] < ymax) {
		data.r <- crop(data.r, extent(ext))
	}

	return(data.r)
	### raster layer 
}, ex=function() {
#	data <- ReadVI3g("geo00oct15a.n14-VI3g")
#	plot(data)
})
