PhenologyNCDF <- structure(function(
	##title<< 
	## Calculate phenology metrics on time series in gridded (raster) data stored in NetCDF files
	##description<<
	## This function calculates metrics of vegetation phenology on multi-temporal raster data. See \code{\link{Phenology}}.
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
	## Tiles of large raster datasets can be processed in parallel by setting the number of nodes.

	file, 
	### multi-layer raster file 
	
	path.out = getwd(),
	### directory for results
	
	start=c(1982, 1),
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12,
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.
	
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
	
	trend = FALSE,
	### Compute trends on the results of phenology analysis? If TRUE, trends will be using \code{\link{TrendAAT}}.
	
	nodes = 1,
	### How many cluster nodes should be used for parallel computing? \code{\link{makeCluster}} and \code{\link{clusterApply}} from the snow package are used for parallel computing. If nodes = 1, parallel computing is not used.
	
	restart = FALSE,
	### load results from files of previously calculated tiles and stack results?

	...
	### additional arguments as for \code{\link{TrendNCDF}}
	
	##references<< 
	## Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334. \cr
	## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674. \cr	
	## White M.A., P.E. Thornton and S.W. Running (1997): A continental phenology model for monitoring vegetation responses to interannual climatic variability. - Global Biogeochemical Cycles 11:217–234. 
	
	##seealso<<
	## \code{\link{PhenologyRaster}}, \code{\link{Phenology}}, \code{\link{PhenologyNCDF}}, \code{\link{NamesPhenologyRaster}} 
) {
		
	# get file name
	filesplit <- unlist(strsplit(file, "/"))
	filename <- filesplit[length(filesplit)]
	filesuffix <- unlist(strsplit(filename, ".", fixed=TRUE))
	filesuffix <- filesuffix[length(filesuffix)] 
	filesuffix <- paste(".", filesuffix, sep="")
	
	# create output directory	
	method <- method[1]
	approach <- approach[1]
	tsgf <- tsgf[1]
	method.str <- paste(tsgf, method[tsgf == "TSGFdoublelog"], approach, sep="_")
	path.out2 <- paste(path.out, gsub(filesuffix, "", filename), method.str, sep="/")
	path.out2 <- gsub("//", "/", path.out2)
	dir.create(path.out2, recursive=TRUE)
	
	# print information 
	message(paste(
		"\n",
		"------------------------------------------------------------------",
		"-- PhenologyNCDF -------------------------------------------------",
		paste("-- start at ", Sys.time()),
		paste("file     :", file), 
		paste("tsgf     :", tsgf),
		paste("method   : ", method, sep="")[tsgf == "TSGFdoublelog"],
		paste("approach : ", approach, sep=""),
		paste("trend    : ", trend, sep=""),
		paste("results  : ", path.out2, sep=""),
		paste("nodes    : ", nodes, sep=""),
		"------------------------------------------------------------------",
		" ",
		sep="\n"))

	# read dataset
	wd <- getwd()
	data.rb <- brick(file)
	message(paste("PhenologyNCDF: Data read.", Sys.time()))
	
	# check for parallel computing
	parallel <- FALSE
	if (nodes > 1) parallel <- TRUE
	
	# define output variables
	vars <- c("SOS", "EOS", "LOS", "POP", "POT", "MGS", "PEAK", "TROUGH", "MSP", "MAU", "RSP", "RAU")
	vars.longname <- c("start of season", "end of season", "length of season", "position of peak", "position of trough", "mean growing season value", "peak value", "trough value", "mean spring value", "mean autumn value", "spring greenup rate", "autumn senescence rate")
	
	# prepare for parallel processing
	if (parallel) {
		# split raster for parallel processing
		message(paste("PhenologyNCDF: Split dataset in tiles for parallel computing.", Sys.time()))
		data.rb.l <- SplitRasterEqually(data.rb, n=nodes)
		id.l <- as.list(1:nodes)
		
		# initialize cluster
		require(snow)
		cluster <- makeCluster(nodes)
		
		# load packages on all nodes
		clusterEvalQ(cluster, {
			library(raster)
			library(ncdf)
			library(plyr)
			library(greenbrown)
			library(Rssa)
			NULL
		})
			
		# export required objects to cluster nodes
		clusterExport(cluster, c("data.rb.l", "start", "freq", "approach", "min.mean", "trs", "fpg", "tsgf", "interpolate", "min.gapfrac", "lower", "fillval", "fun", "method", "filesuffix", "filename", "path.out2", "vars", "vars.longname", "parallel", "restart", "check.seasonality",
		### TODO: exclude the following after greenbrown is compiled:
		"PhenologyRaster", "Phenology", "NamesPhenologyRaster", "TsPP", "PhenoTrs", "PhenoDeriv", "IsPermanentGap", "FillPermanentGaps", "TSGFspline", "TSGFssa", "TSGFdoublelog", "TSGFlinear", "FitDoubleLogElmore", "FitDoubleLogBeck", "Greenup", "Seasonality", "WriteNCDF"), envir=environment())
		message(paste("PhenologyNCDF: Finished preparing cluster nodes for parallel computing.", Sys.time()))	
	} 
	
	# iterate over tiles: calculate phenology and save results
	message(paste("PhenologyNCDF: Start phenology computation.", Sys.time()))
	if (parallel) {
	# using parallel processing
		files.out <- clusterApply(cluster, id.l, function(id) {
			files.out <- NULL
			tile.rb <- data.rb.l[[id]]
			
			# if tile already exists and restart = TRUE, load from file
			setwd(path.out2)
			file.out <- gsub(filesuffix, paste(".", vars[1], ".Tile", id, filesuffix, sep=""), filename)
			if (restart & file.exists(file.out)) {
				for (i in 1:length(vars)) {
					file.out <- gsub(filesuffix, paste(".", vars[i], ".Tile", id, filesuffix, sep=""), filename)
					files.out <- c(files.out, file.out)
				}
			} else {
			# calculate phenology
				phen.rb <- PhenologyRaster(tile.rb, start=start, freq=freq, approach = approach, min.mean = min.mean, trs = trs, fpg = fpg, tsgf = tsgf, interpolate = interpolate, min.gapfrac = min.gapfrac, lower = lower, fillval = fillval, fun = fun, method = method, backup = backup, check.seasonality = check.seasonality)	
					
				# write output variables
				nyears <- nlayers(phen.rb) / length(vars)
				time.out <- as.Date(ts(1:nyears, start=start, frequency=1))
				for (i in 1:length(vars)) {
					file.out <- gsub(filesuffix, paste(".", vars[i], ".Tile", id, filesuffix, sep=""), filename)
					var.rb <- subset(phen.rb, grep(vars[i], names(phen.rb)))
					setwd(path.out2)
					WriteNCDF(var.rb, var.name=vars[i], var.unit="-", var.longname=vars.longname[i], time=time.out, file=file.out, overwrite=TRUE)
					files.out <- c(files.out, file.out)
				}
			
			} # end else restart
			return(files.out)
		}) # end clusterApply
	} else {
	# no parallel processing
		
		# calculate phenology
		phen.rb <- PhenologyRaster(data.rb, start=start, freq=freq, approach = approach, min.mean = min.mean, trs = trs, fpg = fpg, tsgf = tsgf, interpolate = interpolate, min.gapfrac = min.gapfrac, lower = lower, fillval = fillval, fun = fun, method = method, check.seasonality = check.seasonality)	
				
		# write output variables
		nyears <- nlayers(phen.rb) / length(vars)
		time.out <- as.Date(ts(1:nyears, start=start, frequency=1))
		files.out <- NULL
		for (i in 1:length(vars)) {
			file.out <- gsub(filesuffix, paste(".", vars[i], filesuffix, sep=""), filename)
			var.rb <- subset(phen.rb, grep(vars[i], names(phen.rb)))
			setwd(path.out2)
			WriteNCDF(var.rb, var.name=vars[i], var.unit="-", var.longname=vars.longname[i], time=time.out, file=file.out, overwrite=TRUE)
			files.out <- c(files.out, file.out)
		}
	}
	message(paste("PhenologyNCDF: Phenology computation finished. Start saving results.", Sys.time()))

	# stop cluster
	if (parallel) stopCluster(cluster)
	
	# stack results from tiles, write stacked file and remove tiled files
	if (parallel) {
		message(paste("PhenologyNCDF: Start merging tiles.", Sys.time()))
		files.out2 <- NULL
		for (i in 1:length(vars)) {
			# read files for variable for each tile
			setwd(path.out2)
			files.var.l <- llply(files.out, function(files) files[grep(vars[i], files)])
			# ###
			# i <- 1			
			# files.var.l <- as.list(list.files(pattern="SOS"))		
			# files.var.l	
			# ###
			x <- llply(files.var.l, brick)
			
			# merge tiles
			file.out <- gsub(filesuffix, paste(".", vars[i], filesuffix, sep=""), filename)
			files.out2 <- c(files.out2, file.out)
			x$overwrite <- TRUE
			var.rb <- do.call(merge, x)
			
			# write result
			time.out <- as.Date(ts(1:nlayers(var.rb), start=start, frequency=1))
			WriteNCDF(var.rb, var.name=vars[i], var.unit="-", var.longname=vars.longname[i], time=time.out, file=file.out, overwrite=TRUE)
			
			# delete tile files 
			file.remove(unlist(files.var.l))
		}
		files.out <- files.out2
	}
	files.out <- unlist(files.out)
	
	# do trend analysis on phenology results
	if (trend) {
		message(paste("PhenologyNCDF: Start trend analysis.", Sys.time()))
		files.trd <- NULL
		for (i in 1:length(files.out)) {
			setwd(path.out2)
			message(paste("PhenologyNCDF: Start trend analysis for ", files.out[i], ".", Sys.time()))
			files.trd <- c(files.trd, TrendNCDF(files.out[i], start=start, freq=1, ...))
		}
		files.out <- c(files.out, files.trd)
	}
	
	message(paste("PhenologyNCDF: DONE.", Sys.time()))
	setwd(wd)
	return(files.out)
	### The function saves several NetCDF files in a directory on disc. The files are created based on the filename of the input \code{file}:
	### \itemize{ 
	### \item{ \code{file.SOS.nc} file with annual layers of the start of season }
	### \item{ \code{file.EOS.nc} file with annual layers of the end of season  }
	### \item{ and so on for "LOS", "POP", "POT", "MGS", "PEAK", "TROUGH", "MSP", "MAU", "RSP", "RAU"  }
	### }
})

