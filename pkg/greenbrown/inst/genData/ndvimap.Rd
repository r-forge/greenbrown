     \name{ndvimap}
     \docType{data}
     \alias{ndvimap}
\title{Map of Normalized Difference Vegetation Index}
\description{
This dataset is a multi-temporal map of Normalized Difference Vegetation Index (NDVI) from central Alaska. NDVI is a measure of vegetation greenness and is related to the coverage of green vegetation, photosynthetic activity and green biomass. NDVI ranges between -1 and 1. Bare ground and snow has usually NDVI values below 0.2, while vegetated areas have NDVI values above 0.2. Therefore, grid cells with long-term mean NDVI values below 0.2 were masked with NA. This mutli-temporal NDVI map is from the GIMMS (Global Inventory, Monitoring, and Modeling Studies) NDVI3g dataset that provides NDVI estimates from AVHRR (Advanced Very High Resolution Radiometer) satellite observations for 1982-2011. The original dataset has a bi-weekly temporal resolution. Some observations might be affected from snow or cloud cover. Therefore, the bi-weekly NDVI values were aggregated to monthly values using the maximum value. Each layer of this raster brick is a monthly time step between January 1982 and December 2011.
We thank Dr. Compton Tucker, Dr. Jorge Pinzon, Dr. Ranga Myneni and the GIMMS group for producing and providing the GIMMS NDVI3g dataset. Please contact Dr. Compton Tucker if you want to use this dataset for your own research.
}
\usage{
# load the NDVI map
data(ndvimap)	

# print summary
ndvimap	

# plot map
plot(ndvimap)	
plot(ndvimap, 42)				# plot selected time step

# extract time series of a specific grid cell
xy <- cbind(-152, 67)			# coordinates of the grid cell
ndvi.xy <- extract(ndvimap, xy)	# extract NDVI time series for this pixel
ndvi.xy <- as.vector(ndvi.xy)
date <- as.Date(ndvimap@z$Date) # get vector of dates
plot(date, ndvi.xy, type="l")	# plot NDVI time series of the selected pixel

}
\format{A object of class \code{\link{brick}}.}
\references{
Tucker, C.; Pinzon, J.; Brown, M.; Slayback, D.; Pak, E.; Mahoney, R.; Vermote, E.; El Saleous, N., An extended AVHRR 8-km NDVI dataset compatible with MODIS and SPOT VEGETATION NDVI data. International Journal of Remote Sensing 2005, 26, 4485-4498.
}
\keyword{datasets}