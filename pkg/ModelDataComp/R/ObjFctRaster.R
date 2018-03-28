ObjFctRaster <- structure(function(
  ##title<< 
  ## Compute objective functions for raster data
  ##description<<
  ## Calculates several model performance metrics for raster layers per grid cell. 
  
  sim.r,
  ### rasters with simulated/predicted/modeled values
  
  obs.r,
  ### raster with observed/reference values. It is also possible to provide a list of rasters, i.e. if there are multiple reference datasets. In this case, the simulated values will be concatenated and the performance metrics will be computed against all reference datasets at once.
  
  ...
  ### further arguments (not used)
  
  ##details<<
  ## The function is based on \code{\link{ObjFct}} 

  ##seealso<<
  ## \code{\link{ObjFct}}
) { 

  # match layer names and repeat sim in case of multiple observations
  if (!is.list(obs.r)) obs.r <- list(obs.r)
  simobs.l <- llply(obs.r, function(obs.r) {
    r <<- obs.r
    m <- na.omit(match(names(sim.r), names(obs.r)))
    if (length(m) == 0) stop("Layer names of simulations are not included in observations.")
    obs2.r <- raster::subset(obs.r, m)
    m <- na.omit(match(names(obs2.r), names(sim.r)))
    sim2.r <- raster::subset(sim.r, m)
    l <- list(sim=sim2.r, obs=obs2.r)
    return(l)
  })  
  sim.r <- brick(stack(llply(simobs.l, function(l) l$sim)))
  ext <- extent(sim.r)
  obs.l <- llply(simobs.l, function(l) {
    #obs.r <- resample(l$obs, sim.r, method="ngb")
    l$obs
  })
  obs.r <- brick(stack(obs.l))

  # make raster of grid cell coordinates
  xy.r <- raster::subset(sim.r, 1:2)
  xy.r[] <- coordinates(xy.r)

  # iterate over grid cells and compute objective functions
  obj.nms <- names(ObjFct(1:10, 10:1))
  obj.r <- calc(xy.r, fun=function(xy, ...) {
    xy <- cbind(xy[1], xy[2])
    obs <- (obs.r[cellFromXY(obs.r, xy)])[1,]
    sim <- (sim.r[cellFromXY(sim.r, xy)])[1,]
    # ScatterPlot(obs, sim, objfct=TRUE)
    obj <- rep(NA, length(obj.nms)+1)
    so <- na.omit(cbind(sim, obs))
    n <- nrow(so)
    if (n >= 2) obj <- c(n, unlist(ObjFct(so[,1], so[,2])))
    return(obj)
  })
  names(obj.r) <- c("N", obj.nms)
  return(obj.r)
  ### The functions returns a raster brick with different model performance measures. The first layer (N) contains the number of sim/obs pairs in each grid cell. The other layers are model performance measures as computed by \code{\link{ObjFct}}.
}, ex=function() {
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
})
