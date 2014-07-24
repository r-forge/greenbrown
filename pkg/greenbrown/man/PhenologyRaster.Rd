\name{PhenologyRaster}
\alias{PhenologyRaster}
\title{Calculate phenology metrics on time series in gridded (raster) data}
\description{This function calculates metrics of vegetation phenology (start of season, end of season, length of season, peak date) on multi-temporal raster data. 
This function calculates from time series in gridded (raster) data annual metrics of vegetation phenology using the function \code{\link{Phenology}}:
\itemize{ 
\item{ \code{sos} start of season }
\item{ \code{eos} end of season }
\item{ \code{los} length of season }
\item{ \code{pop} position of peak value }
\item{ \code{mgs} mean growing season value }
\item{ \code{peak} peak value }
\item{ \code{msp} mean spring value }
\item{ \code{mau} mean autumn value }
\item{ \code{rsp} rate of spring greenup (not all methods) }
\item{ \code{rau} rate of autumn senescence rates (not all methods) }
The calculation of these metrics is performed in two steps and by using different methods:
\itemize{ 
\item{ Step 1: Time series pre-processing. Before phenology metrics can be estimated, time series need often to be pre-processed. This pre-processing involves filling of permanent and irregular gaps, temporal smoothing and interpolation to daily values. See \code{\link{TsPP}} for details and available methods. }
\item{ Step 2: Estimation of phenology metrics. Phenology metrics are estimated from the gap filled, smoothed and interpolated time series. This can be done by treshold methods (\code{\link{PhenoTrs}}) or by using the derivative of the time series (\code{\link{PhenoDeriv}}). }
}}
\usage{PhenologyRaster(r, start = c(1982, 1), freq = 12, approach = c("White", 
    "Trs", "Deriv"), min.mean = 0.1, trs = NULL, fpg = FillPermanentGaps, 
    tsgf = "TSGFspline", interpolate = TRUE, min.gapfrac = 0.2, 
    lower = TRUE, fillval = NA, fun = min, method = c("Elmore", 
        "Beck"), backup = NULL, ...)}
\arguments{
  \item{r}{multi-layer raster object of class \code{\link[raster]{brick}}}
  \item{start}{beginning of the time series (i.e. the time of the first observation). The default is c(1982, 1), i.e. January 1982 which is the usual start date to compute trends on long-term series of satellite observations of NDVI. See \code{\link[stats]{ts}} for further examples.}
  \item{freq}{The frequency of observations. The default is 12 for monthly observations. Use 24 for bi-monthly observations, 365 for daily observations or 1 for annual observations. See \code{\link{ts}} for further examples.}
  \item{approach}{Approach to be used to calculate phenology metrics from smoothed time series. 'White' by sclaing annual cycles between 0 and 1 (White et al. 1997); 'Trs' for simple tresholds; 'Deriv' by using the derivative of the smoothed function.}
  \item{min.mean}{minimum mean annual value in order to calculate phenology metrics. Use this threshold to suppress the calculation of metrics in grid cells with low average values	}
  \item{trs}{threshold to be used to determine SOS and EOS if method 'Trs' is used. If method 'Trs' is used but trs is NULL than trs will be computed from the long-term mean of Yt.}
  \item{fpg}{Filling of permanent gaps: If NULL, permanent gaps will be not filled, else the function \code{\link{FillPermanentGaps}} will be applied.}
  \item{tsgf}{Temporal smoothing and gap filling: Function to be used for temporal smoothing, gap filling and interpolation of the time series. If NULL, this step will be not applied. Otherwise a function needs to be specified. Exisiting functions that can be applied are \code{\link{TSGFspline}}, \code{\link{TSGFssa}}, \code{\link{TSGFdoublelog}}  }
  \item{interpolate}{Should the smoothed and gap filled time series be interpolated to daily values?}
  \item{min.gapfrac}{How often has an observation to be NA to be considered as a permanent gap? (fraction of time series length) Example: If the month January is 5 times NA in a 10 year time series (= 0.5), then the month January is considered as permanent gap if min.gapfrac = 0.4.	}
  \item{lower}{For filling of permanent gaps: fill lower gaps (TRUE), upper gaps (FALSE) or lower and upper gaps (NULL)}
  \item{fillval}{For filling of permanent gaps: constant fill values for gaps. If NA the fill value will be estimated from the data using fun. }
  \item{fun}{For filling of permanent gaps: function to be used to compute fill values. By default, minimum.}
  \item{method}{If 'tsgf' is TSGFdoublelog: Which kind of double logistic curve should be used to smooth the data? 'Elmore' (Elmore et al. 2012) or 'Beck' (Beck et al. 2006).	}
  \item{backup}{Which backup algorithm should be used instead of TSGFdoublelog for temporal smoothing and gap filling if the time series has no seasonality? If a time series has no seasonal pattern, the fitting of double logistic functions is not meaningful. In this case another method can be used. Default: NULL (returns NA - no smoothing), other options: "TSGFspline", "TSGFssa", "TSGFlinear"	}
  \item{\dots}{additional arguments as for \code{\link{writeRaster}}}
}

\value{The function returns a RasterBrick with different phenology metrics statistics. The layers are named:
\itemize{ 
\item{ \code{SOS.} start of season in year x }
\item{ \code{EOS.} end of season in year x }
\item{ \code{LOS.} length of season in year x }
\item{ \code{POP.} position of peak in year x }
\item{ \code{MGS.} mean growing season value in year x }
\item{ \code{PEAK.} peak value in year x }
\item{ \code{MSP.} mean spring value in year x }
\item{ \code{MAU.} mean autumn value in year x }
\item{ \code{RSP.} rate of spring greenup in year x (only if approach is 'Deriv') }
\item{ \code{RAU.} rate of autumn senescence in year x (only if approach is 'Deriv') }
}
The number of years in the input raster will define the number of layers in the result.  }
\references{Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.
Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.		
White M.A., P.E. Thornton and S.W. Running (1997): A continental phenology model for monitoring vegetation responses to interannual climatic variability. - Global Biogeochemical Cycles 11:217–234.}
\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}



\seealso{\code{\link{Phenology}}, \code{\link{PhenologyNCDF}}, \code{\link{NamesPhenologyRaster}} }
\examples{
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
data(ndvimap)
plot(ndvimap, 8)

# calculate phenology metrics: (this can take some time!)
phenmap <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFspline", approach="Deriv") 
# Select method by defining 'tsgf' (temporal smoothing and gap filling) and by 'approach' (method to summarize phenology metrics). See \code{\link{Phenology}} for examples and a comparison of methods.
plot(phenmap)
plot(phenmap, grep("SOS.1982", names(phenmap))) # start of season 1982
plot(phenmap, grep("EOS.1982", names(phenmap))) # end of season 1982
plot(phenmap, grep("LOS.1982", names(phenmap))) # length of season 1982
plot(phenmap, grep("POP.1982", names(phenmap))) # position of peak value 1982
plot(phenmap, grep("MGS.1982", names(phenmap))) # mean growing season value 1982
plot(phenmap, grep("PEAK.1982", names(phenmap))) # peak value 1982
plot(phenmap, grep("MSP.1982", names(phenmap))) # mean spring value 1982
plot(phenmap, grep("MAU.1982", names(phenmap))) # mean autumn value 1982
plot(phenmap, grep("RSP.1982", names(phenmap))) # rate of spring greenup 1982
plot(phenmap, grep("RAU.1982", names(phenmap))) # rate of autumn senescence 1982

# calculate trends on length of season using TrendRaster
losmap <- subset(phenmap, grep("LOS", names(phenmap)))
plot(losmap)
lostrend <- TrendRaster(losmap, start=c(1982, 1), freq=1)
plot(lostrend)

# classify trends in length of season
lostrend.cl <- TrendClassification(lostrend)
plot(lostrend.cl, col=brgr.colors(3), breaks=c(-1.5, -0.5, 0.5, 1.5))	# only a few pixels have a positive trend in the length of growing season

# # compare different methods
# # section is uncommented because examples take some long time:
# lin.trs <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFlinear", approach="White") 
# lin.deriv <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFlinear", approach="Deriv") 
# spl.trs <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFspline", approach="White") 
# spl.deriv <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFspline", approach="Deriv") 
# ssa.trs <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFssa", approach="White") 
# ssa.deriv <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFssa", approach="Deriv") 
# beck.trs <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFdoublelog", approach="White", method="Beck") 
# beck.deriv <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFdoublelog", approach="Deriv", method="Beck") 
# elmore.trs <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFdoublelog", approach="White", method="Elmore") 
# elmore.deriv <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFdoublelog", approach="Deriv", method="Elmore") 

# # compare patterns of SOS
# phenvar <- "SOS.1982"
# phenmulti.rb <- brick(stack(raster(lin.trs, grep(phenvar, names(lin.trs))),	raster(lin.deriv, grep(phenvar, names(lin.trs))), raster(spl.trs, grep(phenvar, names(lin.trs))), raster(spl.deriv, grep(phenvar, names(lin.trs))), raster(ssa.trs, grep(phenvar, names(lin.trs))), raster(ssa.deriv, grep(phenvar, names(lin.trs))), raster(beck.trs, grep(phenvar, names(lin.trs))), raster(beck.deriv, grep(phenvar, names(lin.trs))), raster(elmore.trs, grep(phenvar, names(lin.trs))), raster(elmore.deriv, grep(phenvar, names(lin.trs)))))	
# names(phenmulti.rb) <- c("lin.trs", "lin.deriv", "spl.trs", "spl.deriv", "ssa.trs", "ssa.deriv", "beck.trs", "beck.deriv", "elmore.trs", "elmore.deriv")
# plot(phenmulti.rb, zlim=range(values(phenmulti.rb), na.rm=TRUE), col=tim.colors(20))

# m <- na.omit(values(phenmulti.rb))
# cor(m)

citation("greenbrown")
}
