\name{ObjFctRaster}
\alias{ObjFctRaster}
\title{Compute objective functions for raster data}
\description{Calculates several model performance metrics for raster layers per grid cell. }
\usage{ObjFctRaster(sim.r, obs.r, ...)}
\arguments{
  \item{sim.r}{rasters with simulated/predicted/modeled values}
  \item{obs.r}{raster with observed/reference values. It is also possible to provide a list of rasters, i.e. if there are multiple reference datasets. In this case, the simulated values will be concatenated and the performance metrics will be computed against all reference datasets at once.}
  \item{\dots}{further arguments (not used)}
}
\details{The function is based on \code{\link{ObjFct}} }
\value{The functions returns a raster brick with different model performance measures. The first layer (N) contains the number of sim/obs pairs in each grid cell. The other layers are model performance measures as computed by \code{\link{ObjFct}}.}

\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{ObjFct}}}
\examples{
# create some example data for observations and simulations 
obs.r <- brick(system.file("external/rlogo.grd", package="raster"))
obs.r[obs.r < 20] <- NA
sim.r <- obs.r 
sim.r[] <- values(sim.r) * rnorm(length(obs.r), 0.98, 0.2)  
plot(obs.r)
plot(sim.r)

# compute objective functions
obj.r <- ObjFctRaster(sim.r, obs.r)
plot(obj.r, 1:9)
plot(obj.r, 10:18)
plot(obj.r, 19:27)
plot(obj.r, 28:34)

# example with two reference datasets
obs2.r <- obs.r
obs2.r[] <- values(obs.r) * rnorm(length(obs.r), 1, 0.01)  
obj.r <- ObjFctRaster(sim.r, list(obs.r, obs2.r))
plot(obj.r, 1:9)

# example with different number of layers, e.g. different temporal coverage
sim.r <- brick(stack(sim.r, sim.r))
names(sim.r) <- seq(as.Date("2000-01-01"), as.Date("2005-01-01"), by="year")
names(obs.r) <- seq(as.Date("2002-01-01"), as.Date("2004-01-01"), by="year")
obj.r <- ObjFctRaster(sim.r, obs.r)
plot(obj.r, 1:9)
}
