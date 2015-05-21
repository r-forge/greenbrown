TrendNCDF <- structure(function(
	##title<< 
	## Calculate trends and trend statistics on time series in gridded (raster) data stored in a NetCDF file
	##description<<
	## This function computes temporal trend and trend breakpoints on multi-temporal raster data that is stored in a NetCDF file. To calculate trends on the values of each grid cell the function \code{\link{Trend}} is used. Before using these methods on satellite time series (especially NDVI time series) the descriptions and recommendations in Forkel et al. (2013) should be considered. The function applies the function \code{\link{TrendRaster}} on a NetCDF file and saves the results as NetCDF files. Additionally, several summary raster layers are saved as NetCDF files too. Thus, it can potentially simplify the workflow.

	file, 
	### NetCDF file with file extention *.nc 
	
	start=c(1982, 1), 
	### beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.
	
	freq=12,
	### The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.	
	
	method="AAT",
	### method to be used for trend calculation with the following options: 
	### \itemize{
	### \item{ \code{AAT} (default) calculates trends on annual aggregated time series (see \code{\link{TrendAAT}} for details). This method will be automatically choosen if the time series has a frequency of 1 (e.g. in case of annual time steps). If the time series has a frequency > 1, the time series will be aggregated to annual time steps using the mean. }
	### \item{ \code{STM} fits harmonics to the seasonal time series to model the seasonal cycle and to calculate trends based on a multiple linear regression (see \code{\link{TrendSTM}} for details). }
	### \item{ \code{SeasonalAdjusted} removes first the seasonal cycle from the time series and calculates the trend on the reaminder series (see \code{\link{TrendSeasonalAdjusted}} for details). }
	### }	
	
	mosum.pval = 0.05, 
	### Maximum p-value for the OLS-MOSUM test in order to search for breakpoints. If p = 0.05, breakpoints will be only searched in the time series trend component if the OLS-MOSUM test indicates a significant structural change in the time series. If p = 1 breakpoints will be always searched regardless if there is a significant structural change in the time series or not. See \code{\link[strucchange]{sctest}} for details.	
	
	h = 0.15, 
	### minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment. See \code{\link[strucchange]{breakpoints}} for details.
	
	breaks = 1,
	### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.
	
	funSeasonalCycle=MeanSeasonalCycle,
	### a function to estimate the seasonal cycle of the time series if \code{SeasonalAdjusted} is selected as method. An own function can be defined to estimate the seasonal cycle which has to return the seasonal cycle as a time series of class \code{\link{ts}}. Currently two approaches are part of this package:
	### \itemize{ 
	### \item{ \code{\link{MeanSeasonalCycle}} is the default which computes the mean seasonal cycle. }
	### \item{ \code{\link{SSASeasonalCycle}} detects a modulated seasonal cycle based on Singular Spectrum Analysis. }
	### }
	
	funAnnual=mean,
	### function to aggregate time series to annual values if \code{AAT} is selected as method. The default function is the mean (i.e. trend calculated on mean annual time series). See \code{TrendAAT} for other examples
	
	...
	
	##details<<
	## The maximum number of breakpoints should be specified in this function. If \code{breaks=0} no breakpoints will be computed. If \code{breaks=1} one breakpoint can be detected at maximum per grid cell. In this case the result will be reported for two time series segments (SEG1 before the breakpoint, SEG2 after the breakpoint). 
	## Some of the trend methods are very slow. Applying them on multi-temporal raster datasets can take some time. Especially the methods that work on the full temporal resolution time series (STM and SeasonalAdjusted) are slower than the method AAT. Especially if breakpoints are computed the computations take longer. The computation of breakpoints can be suppressed by choosing breaks=0. For large rasters it is recommended to first split the raster dataset in several tiles and to compute the trends on each tile separately. The use of a high performance computing infrastructure it also advantageous. 
	## All methods work with missing observations (for example missing NDVI observation in winter months with snow cover). Missing observation have to be flagged with NA. All time steps have to be included in the RasterBrick for trend analysis. If complete time steps are missing, they need to be included as layers (filled with NA values) in the RasterBrick to form a continuous time series. 

	##references<< Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing 5.	
	
	##seealso<<
	## \code{\link{TrendRaster}}, \code{\link{Trend}}, \code{\link{TrendClassification}}, \code{\link{TrendLongestSEG}}, \code{\link{TrendSegmentsRaster}}, \code{\link{NamesTrendRaster}} 	
	) {
	

	# read dataset
	wd <- getwd()
	file <- gsub("//", "/", file)
	if (!grepl(wd, file)) {
		file <- paste(wd, file, sep="/")
		if (!file.exists(file)) stop(paste("TrendNCDF:", file, "does not exist in working directory."))
	} 
	data.rb <- brick(file)
	
	# create output directory
	
	path.out <- gsub(".nc", "_Trend", file)
	dir.create(path.out)
	setwd(path.out)
	path.out2 <- paste(path.out, "/", "Trend_", method, "_mosum.pval-", mosum.pval, "_h-", h, "_breaks-", breaks, sep="")
	dir.create(path.out2)
	setwd(path.out2)
	file <- unlist(strsplit(file, "/"))
	file <- file[length(file)]
	
	# calculate trend
	trend.rb <- TrendRaster(data.rb, start=start, freq=freq, method=method, mosum.pval=mosum.pval, h=h, breaks=breaks, funAnnual=funAnnual, funSeasonalCycle=funSeasonalCycle)
	
	# write trend result
	file.trd <- gsub(".nc", ".Trend.nc", file)
	WriteNCDF(trend.rb, var.name="Trend", var.unit="-", var.longname="Trend", layernames=names(trend.rb), file=file.trd, overwrite=TRUE)
	
	# classify trend
	trendcl.rb <- TrendClassification(trend.rb, min.length=8, max.pval=0.05)	
	file.trdcl <- gsub(".nc", ".Trend.Classif.nc", file)
	WriteNCDF(trendcl.rb, var.name="Trend.Classif", var.unit="-", var.longname="Trend.Classif", layernames=names(trendcl.rb), file=file.trdcl, overwrite=TRUE)
	
	if (breaks > 0) {
		# breakpoints
		bp.r <- raster::subset(trend.rb, grep("BP", names(trend.rb)))
		file.bp <- gsub(".nc", ".Trend.BP.nc", file)
		WriteNCDF(bp.r, var.name="Trend.BP", var.unit="-", var.longname="Trend.BP", layernames=names(bp.r), file=file.bp, overwrite=TRUE)
	
		# slope and pvalue of longest trend segment
		trend.longestseg.rb <- TrendLongestSEG(trend.rb)
		file.trdlongest <- gsub(".nc", ".Trend.LongestSEG.nc", file)
		WriteNCDF(trend.longestseg.rb, var.name="Trend.LongestSEG", var.unit="-", var.longname="Trend.LongestSEG", layernames=names(trend.longestseg.rb), file=file.trdlongest, overwrite=TRUE)		
		
		sl.longestseg.r <- raster(trend.longestseg.rb, 2)
		file.sllongest <- gsub(".nc", ".Trend.SlopeLongestSEG.nc", file)
		WriteNCDF(sl.longestseg.r, var.name="Trend.SlopeLongestSEG", var.unit="-", var.longname="Trend.SlopeLongestSEG", file=file.sllongest, overwrite=TRUE)				

		pval.longestseg.r <- raster(trend.longestseg.rb, 3)
		file.pvallongest <- gsub(".nc", ".Trend.PvalLongestSEG.nc", file)
		WriteNCDF(pval.longestseg.r, var.name="Trend.PvalLongestSEG", var.unit="-", var.longname="Trend.PvalLongestSEG", file=file.pvallongest, overwrite=TRUE)			
		
		# classify trend of longest segment
		trendcl.longestseg.rb <- TrendClassification(trend.longestseg.rb)
		file.trdlongestcl <- gsub(".nc", ".Trend.LongestSEGClassif.nc", file)
		WriteNCDF(trendcl.longestseg.rb, var.name="Trend.LongestSEGClassif", var.unit="-", var.longname="Trend.LongestSEGClassif", file=file.trdlongestcl, overwrite=TRUE)			
		
	} else {
		file.bp <- NA
		file.trdlongest <- NA
		file.sllongest <- NA
		file.pvallongest <- NA
		file.trdlongestcl <- NA
	}
	result <- c(file.trd, file.trdcl, file.bp, file.trdlongest, file.sllongest, file.pvallongest, file.trdlongestcl)
	setwd(wd)
	return(result)
	### The function saves several NetCDF files in directory on disc. The files are created based on the filename of the input \code{file}:
	### \itemize{ 
	### \item{ \code{file.Trend.nc} NetCDF file with result of trend and breakpoints detection (from \code{\link{TrendRaster}}) }
	### \item{ \code{file.Trend.Classif.nc} NetCDF file with classified trends in each time series segment (from \code{\link{TrendClassification}}) }
	### \item{ \code{file.Trend.BP.nc} NetCDF file with time of breakpoints  }
	### \item{ \code{file.Trend.LongestSEG.nc} NetCDF file with slope and p-values of the longest time series segment (from \code{\link{TrendLongestSEG}}) }
	### \item{ \code{file.Trend.SlopeLongestSEG.nc} NetCDF file with slope of the longest time series segment (from \code{\link{TrendLongestSEG}}) }	
	### \item{ \code{file.Trend.PvalLongestSEG.nc} NetCDF file with p-value of the longest time series segment (from \code{\link{TrendLongestSEG}}) }
	### \item{ \code{file.Trend.LongestSEGClassif.nc} NetCDF file with classified trend of the longest time series segment (i.e.  \code{\link{TrendClassification}} applied on \code{\link{TrendLongestSEG}}) }
	### }
})