\name{WriteNCDF}
\alias{WriteNCDF}
\title{Write raster objects to NetCDF files}
\description{This function writes raster layers to NetCDF files including meta information as variable names and units and time axes.}
\usage{WriteNCDF(data = NULL, var.name, var.unit, var.longname = "", 
    file = NULL, time = NULL, layernames = NULL, data.name = NA, 
    region.name = NA, file.title = var.longname, file.description = NULL, 
    reference = "", provider = "", creator = "greenbrown R package", 
    naflag = -9999, scale = 1, offset = 0, overwrite = FALSE)}\arguments{
  \item{data}{raster layer or raster brick}
  \item{var.name}{variable name}
  \item{var.unit}{variable unit}
  \item{var.longname}{variable long name }
  \item{file}{file name. If NULL the file name will be created from the variable name and the dimensions of the data.}
  \item{time}{vector of time steps for each layer.}
  \item{layernames}{layer names if layers are not time steps.}
  \item{data.name}{name of the dataset}
  \item{region.name}{name of the region}
  \item{file.title}{title of the file}
  \item{file.description}{description of the file}
  \item{reference}{reference for the dataset}
  \item{provider}{dataset provider}
  \item{creator}{dataset creator}
  \item{naflag}{flag for NA values}
  \item{scale}{sclaing values for the data}
  \item{offset}{offset value}
  \item{overwrite}{overwrite existing file?}
}



\author{Matthias Forkel <mforkel@bgc-jena.mpg.de>}





