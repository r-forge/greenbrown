\name{firedata}
\docType{data}
\alias{firedata}
\title{Data on burned area, land cover, climate, vegetation, and socioeconomics }
\description{
This data.frame contains several datasets on burned area, climate, soil moisture, vegetation, land cover, and socioeconomic variables. The data was extracted for sampled 0.25° grid cells of the globe and has monthly time steps. The data includes the following variables:

\itemize{ 
\item{ cellid: identifier of grid cells }
\item{ lon: longitude }
\item{ lat: latitude }
\item{ regid: identifier for regions }
\item{ time: time (months between 1997 and 2011)  }
\item{ area: area of grid cells (ha) }
\item{ train: indicator if data is used for training (train=1) or evaluation (train=4) }
\item{ GFED burned area version 4 (Giglio et al., 2013), http://www.globalfiredata.org }
\itemize{ 
\item{ GFED.BA.obs: Fractional burned area of a 0.25° grid cell, used for optimization of SOFIA models }
\item{ GFED.BA.unc: uncertainty of burned area }
}
\item{ ESA land cover_cci version 1.6.1, http://maps.elie.ucl.ac.be/CCI/viewer/index.php }
\itemize{ 
\item{ CCI.LC.Tree.BE: broadleaved evergreen tree }
\item{ CCI.LC.Tree.BD: Broadleaved deciduous trees }
\item{ CCI.LC.Tree.NE: Needle-leaved evergreen trees }
\item{ CCI.LC.Tree.ND: Needle-leaved deciduous trees }
\item{ CCI.LC.Shrub.BE: Broadleaved evergreen shrubs }
\item{ CCI.LC.Shrub.BD: Broadleaved deciduous shrubs }
\item{ CCI.LC.Shrub.NE: Needle-leaved evergreen shrubs }
\item{ CCI.LC.Herb: Natural grass and herbaceous vegetation }
\item{ CCI.LC.Crop: Cropland and managed grass }
\item{ CCI.LC.HrbCrp: Natural and managed grass and croplands = Herb + Crop }
\item{ CCI.LC.Tree: Coverage of trees = Tree.BE + Tree.BD + Tree.NE + Tree.ND }
\item{ CCI.LC.Shrub: Coverage of shrubs = Shrub.BE + Shrub.BD + Shrub.NE }
\item{ CCI.LC.Broadleaf: Coverage of broadleaved vegetation = Tree.BE + Tree.BD + Shrub.BE + Shrub.BD }
\item{ CCI.LC.Needleleaf: Coverage of needle-leaved vegetation = Tree.NE + Tree.ND + Shrub.NE }
}
\item{ CRU TS3.23 climate data (Harris et al., 2014), https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_3.23 }
\itemize{ 
\item{ CRU.T.orig: Mean monthly air temperature (degC) }
\item{ CRU.T.annual: Mean air temperature in the actual month and the 12 months before a fire }
\item{ CRU.WET.orig: Monthly number of wet day }
\item{ CRU.WET.annual: Mean number of wet days in the actual month and the 12 months before a fire }
\item{ CRU.DTR.orig: Mean monthly diurnal temperature range (K) }
}
\item{ GPCC precipitation version 7, http://dx.doi.org/10.5676/DWD_GPCC/FD_M_V7_050 }
\itemize{ 
\item{ GPCC.P.orig: Monthly total precipitation (mm) }
\item{ GPCC.P.annual: Total precipitation in the actual month and the 12 months before a fire }
}
\item{ ESA soil moisture_cci version 2.3 (Dorigo et al.) , http://cci.esa.int/data }
\itemize{ 
\item{ CCI.SM.orig Mean monthly surface soil moisture }
\item{ CCI.SM.annual Mean monthly surface soil moisture }
}
\item{ GIMMS fraction of absorbed photosynthetic active radiation version 3g (Zhu et al., 2013), http://cliveg.bu.edu/modismisr/lai3g-fpar3g.html }
\itemize{ 
\item{ GIMMS.FAPAR.orig: Mean monthly FAPAR }
\item{ GIMMS.FAPAR.pre: FAPAR in the month before a fire }
\item{ GIMMS.FAPAR.annual: Mean FAPAR in the 12 months before a fire }
}
\item{ Multi-sensor harmonized vegetation optical depth (Liu et al., 2011b, 2015) }
\itemize{ 
\item{ Liu.VOD.orig: Mean monthly VOD }
\item{ Liu.VOD.pre: VOD in the month before a fire }
\item{ Liu.VOD.annual: Mean VOD in the 12 months before a fire }
}
\item{ GRUMP population density version 1 (years 1990, 1995, 2000) (Balk et al., 2006), http://dx.doi.org/10.7927/H4R20Z93 }
\itemize{ 
\item{ PD.med: Population density (individuals km-2), median estimate of three methods for temporal inter- and extrapolation (spline interpolation, linear interpolation, interpolation with last value as constant) }
}
\item{ Night light development index (year 2006) (Elvidge et al., 2012), http://ngdc.noaa.gov/eog/dmsp/download_nldi.html }
\itemize{ 
\item{ NLDI: Night light development index, but grid cells without night lights or population set to 1.01 }
}
}

}
\format{A \code{\link{data.frame}}.}
\references{
Forkel, M., et al. (2016): in prep.
}
\examples{
require(ModelDataComp)

data(firedata)
colnames(firedata)
dim(firedata)

# spatial distribution of the data
ScatterPlot(firedata$lon, firedata$lat, fit.global=FALSE, plot.type="points")

# latitudinal pattern of burned area
fit <- ScatterPlot(firedata$lat, firedata$GFED.BA.obs)

}

\keyword{datasets}
