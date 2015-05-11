\name{ndvi}
\docType{data}
\alias{ndvi}
\title{Time series of Normalized Difference Vegetation Index}
\description{
       This is an example time series of Normalized Difference Vegetation Index (NDVI) from a grid cell in central Alaska. NDVI is a measure of vegetation greenness and is related to the coverage of green vegetation, photosynthetic activity and green biomass. NDVI ranges between -1 and 1. Bare ground and snow has usually NDVI values below 0.2 while vegetated areas have NDVI values above 0.2. This NDVI time series is from the GIMMS (Global Inventory, Monitoring, and Modeling Studies) dataset (Tucker et al. 2005) that provides NDVI estimates from AVHRR (Advanced Very High Resolution Radiometer) satellite observations. 
}
\format{A object of class \code{\link{ts}}.}
\references{
Tucker, C.; Pinzon, J.; Brown, M.; Slayback, D.; Pak, E.; Mahoney, R.; Vermote, E.; El Saleous, N., An extended AVHRR 8-km NDVI dataset compatible with MODIS and SPOT VEGETATION NDVI data. International Journal of Remote Sensing 2005, 26, 4485-4498.
}
\examples{
data(ndvi)
ndvi
plot(ndvi)
}

\keyword{datasets}