WriteNCDF <- structure(function(
	##title<< 
	## Write raster objects to NetCDF files
	##description<<
	## This function writes raster layers to NetCDF files including meta information as variable names and units and time axes.
	
	data=NULL, 
	### raster layer or raster brick
	
	var.name, 
	### variable name
	
	var.unit, 
	### variable unit
	
	var.longname="",
	### variable long name 
	
	file=NULL, 
	### file name. If NULL the file name will be created from the variable name and the dimensions of the data.
	
	time=NULL, 
	### vector of time steps for each layer.
	
	layernames=NULL,
	### layer names if layers are not time steps.
	
	data.name=NA, 
	### name of the dataset
	
	region.name=NA,
	### name of the region
	
	file.title=var.longname, 
	### title of the file
	
	file.description=NULL, 
	### description of the file
	
	reference="", 
	### reference for the dataset
	
	provider="", 
	### dataset provider
	
	creator="greenbrown R package",
	### dataset creator
	
	naflag=-9999, 
	### flag for NA values
	
	scale=1, 
	### sclaing values for the data
	
	offset=0,
	### offset value
	
	overwrite=FALSE
	### overwrite existing file?
) {
	
	require(ncdf)

	# check parameters
	if (is.null(data)) stop("Provide a Raster* object")
	if (is.null(var.name)) stop("Define 'var.name'")
	if (is.null(var.unit)) stop("Define 'var.unit'")
	if (is.null(file.description)) file.description <- paste(var.name, " (", var.longname, ") [", var.unit, "] from ", data.name, sep="")
	
	# check or create time parameters
	if (((nlayers(data) == 1) | (!is.null(layernames))) & is.null(time)) {
		timestep <- 1
		time2 <- 1
		start <- NA
		end <- NA
	} else {
		if (is.null(time)) {
			if (nlayers(data) > 1) {
				stop("Define vector 'time' with time steps.")
			} else {
				time <- end <- start <- timestep <- NA
			}
		} else if ((class(time) == "Date") | (class(time) == "POSIXct") | (class(time) == "POSIXlt") ) {
			start <- try(format(time[1], "%Y"), TRUE)
			if (class(start) == "try-error") start <- time[1]
			end <- try(format(time[length(time)], "%Y"), TRUE)
			if (class(end) == "try-error") end <- time[length(time)]
			timestep <- diff(time)
			if (length(timestep) == 0) {
				timestep <- 0
				time2 <- time
			} else {
				timestep <- gsub(" ", "", format(mean(timestep), digits=1))	
				time2 <- try(difftime(time, as.POSIXct(ISOdatetime(1582,10,14,0,0,0)), units="days"), TRUE)
			}
			if (class(time2) == "try-error") time2 <- time
		} else {
			stop("time should be of class Date, POSIXct or POSIXlt.")
		}
	}

	# create file name and check if file exists
	if (is.null(file)) {
		file <- paste(data.name, var.name, nrow(data), ncol(data), region.name, start, end, timestep, "nc", sep=".")
		file <- gsub("NA.", "", file, fixed=TRUE)
	}
	if (file.exists(file) & (overwrite == FALSE)) stop("File 'file' already exists. Use overwrite=TRUE to overwrite it.")
		
	file.tmp <- paste(tempfile(tmpdir=getwd()), ".grd", sep="")
	# check data and add scale and offset
	if (!is.null(scale) | !is.null(offset) | !is.null(naflag)) {
		data <- calc(data, function(x) { 
			x <- x * scale + offset
			x[is.na(x)] <- naflag
			return(x) 
		}, file=file.tmp)
	}
	
	
	# set dimensions and write values
	#--------------------------------
	
	if (!is.null(layernames)) {
		xy <- xyFromCell(data, 1:ncell(data))
		lon.dim <- dim.def.ncdf("longitude", "degrees_east", unique(xy[,1]))
		lat.dim <- dim.def.ncdf("latitude", "degrees_north", unique(xy[,2]))
		z.dim <- dim.def.ncdf("layer", "", 1:nlayers(data), unlim=TRUE)
		var.ncdf <- var.def.ncdf(name=as.character(var.name), units=as.character(var.unit), 
			dim=list(lon.dim, lat.dim, z.dim), missval=naflag, longname=as.character(var.longname))
		nc <- create.ncdf(file, var.ncdf, verbose=FALSE)
		put.var.ncdf(nc, as.character(var.name), values(data), start=NA, count=NA, verbose=FALSE ) 
		close.ncdf(nc)	
			
	} else {

		# write NetCDF file
		xy <- xyFromCell(data, 1:ncell(data))
		lon.dim <- dim.def.ncdf("longitude", "degrees_east", unique(xy[,1]))
		lat.dim <- dim.def.ncdf("latitude", "degrees_north", unique(xy[,2]))
		time.dim <- dim.def.ncdf("time", "days since 1582-10-14 00:00:00", time2, unlim=TRUE)
		var.ncdf <- var.def.ncdf(name=as.character(var.name), units=as.character(var.unit), 
			dim=list(lon.dim, lat.dim, time.dim), missval=naflag, longname=as.character(var.longname))
		nc <- create.ncdf(file, var.ncdf, verbose=FALSE)
		put.var.ncdf(nc, as.character(var.name), values(data), start=NA, count=NA, verbose=FALSE ) 
		close.ncdf(nc)
	}

	# write file information
	#-----------------------

	nc <- open.ncdf(file, write=TRUE)
	att.put.ncdf(nc, var.name, "missing_value", naflag, "double")
	att.put.ncdf(nc, var.name, "scale_factor", scale, "double")
	att.put.ncdf(nc, var.name, "add_offset", offset, "double")

	# set global attributes
	att.put.ncdf(nc, 0, "title", as.character(file.title), "text")
	att.put.ncdf(nc, 0, "description", as.character(file.description), "text")
	history <- paste(Sys.time(), creator, ": file created from R function WriteNCDF")
	att.put.ncdf(nc, 0, "history", as.character(history), "text")
	att.put.ncdf(nc, 0, "provided_by", as.character(provider), "text")
	att.put.ncdf(nc, 0, "created_by", as.character(creator), "text")
	att.put.ncdf(nc, 0, "reference", as.character(reference), "text")
	
	# write time attributes
	if (length(time) >= 2) {	# if more than two dimensions [lon,lat,time]
		put.var.ncdf(nc, "time", time2) 
		att.put.ncdf(nc, "time", "units", "days since 1582-10-14 00:00", "double")
		att.put.ncdf(nc, "time", "calendar", "gregorian", "text")
	}
	
	# write layernames
	if (!is.null(layernames)) {
		put.var.ncdf(nc, "layer", 1:length(layernames)) 
		att.put.ncdf(nc, "layer", "description", layernames, "text")
	}
	close.ncdf(nc)
		
	# delete temporary files
	file.remove(file.tmp)
	file.remove(gsub(".grd", ".gri", file.tmp))
	return(nc)
})




