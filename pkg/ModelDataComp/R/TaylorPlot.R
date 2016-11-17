TaylorPlot <- structure(function(
	##title<< 
	## Plot a Taylor diagram
	##description<<
	## Plot a Taylor diagram. This is a modification of the function \code{\link{taylor.diagram}} in the plotrix package.
	
	sim,
	### simulated/predicted/modeled values
	
	obs,
	### observed/reference values
	
	groups=rep(1, length(sim)),
	### numeric vector that split ref and model in different groups, e.g. to indicate different models or subsets of the data
	
	col = NULL,
	### color for each group, should have the same length as grouping elements
	
	plot.combined = FALSE,
	### plot also the combined set, i.e. without splitting by groups?
	
	normalize = TRUE, 
	### whether to normalize the models so that the reference has a standard deviation of 1
	
	sd.arcs = TRUE, 
	### whether to display arcs along the standard deviation axes 
	
	text.groups = NULL,
	### text labels for the groups
	
	text.obs = "Obs",
	### text label for the observations
	
	text.combined = "All",
	### text label for the combined model/data set without grouping
	
	pos.cor = TRUE,
	### whether to display only positive (TRUE) or all values of correlation (FALSE). If NULL, this will depend on the correlations.
	
	...
	### further arguments as in \code{\link{taylor.diagram}} 

	##details<<
	## No details.
	
	##references<< Taylor, K.E., 2001. Summarizing multiple aspects of model performance in a single diagram. Journal of Geophysical Research 106, 7183-7192.
	
	##seealso<<
	## \code{\link{ObjFct}}, \code{\link{plot.ObjFct}}, \code{\link{WollMilchSauPlot}}
) { 
   ### original taylor diagram function with added 'text' argument
   .taylor.diagram <- function (obs, sim, add = FALSE, col = "red", pch = 19, pos.cor = TRUE, 
       xlab = "", ylab = "", main = "Taylor Diagram", show.gamma = TRUE, 
       ngamma = 3, gamma.col = 8, sd.arcs = 0, obs.sd = FALSE, sd.method = "sample", 
       grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1.2, 
       normalize = FALSE, mar = c(5, 4, 6, 6), text=NULL, xlim=NULL, ...) {
       grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
           1)
       R <- cor(obs, sim, use = "pairwise")
       if (is.list(obs)) 
           obs <- unlist(obs)
       if (is.list(sim)) 
           obs <- unlist(sim)
       SD <- function(x, subn) {
           meanx <- mean(x, na.rm = TRUE)
           devx <- x - meanx
           ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
               subn))
           return(ssd)
       }
       subn <- sd.method != "sample"
       sd.r <- SD(obs, subn)
       sd.f <- SD(sim, subn)
       if (normalize) {
           sd.f <- sd.f/sd.r
           sd.r <- 1
       }
       maxsd <- 1.05 * max(c(sd.f, sd.r), na.rm=TRUE)
       oldpar <- par("mar", "xpd", "xaxs", "yaxs")
       if (!add) {
           if (pos.cor) {
               if (nchar(ylab) == 0) 
                   ylab = "Standard deviation"
               par(mar = mar, cex.lab=1.3, mgp=c(2.4, 1, 0), cex.main=1.3)
               xlim <- c(0, maxsd)
               plot(0, xlim = xlim, ylim = xlim, xaxs = "i", 
                   yaxs = "i", axes = FALSE, main = main, xlab = xlab, 
                   ylab = ylab, type = "n", cex = cex.axis, ...)
               if (grad.corr.lines[1]) {
                   for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                     gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
               }
               segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                   0))
               axis.ticks <- pretty(c(0, maxsd))
               axis.ticks <- axis.ticks[axis.ticks <= maxsd]
               axis(1, at = axis.ticks, cex.axis = cex.axis)
               axis(2, at = axis.ticks, cex.axis = cex.axis)
               if (sd.arcs[1]) {
                   if (length(sd.arcs) == 1) 
                     sd.arcs <- axis.ticks
                   for (sdarc in sd.arcs) {
                     xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
                     ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
                     lines(xcurve, ycurve, col = "blue", lty = 3)
                   }
               }
               if (show.gamma[1]) {
                   if (length(show.gamma) > 1) 
                     gamma <- show.gamma
                   else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
                   if (gamma[length(gamma)] > maxsd) 
                     gamma <- gamma[-length(gamma)]
                   labelpos <- seq(45, 70, length.out = length(gamma))
                   for (gindex in 1:length(gamma)) {
                     xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + 
                       sd.r
                     endcurve <- which(xcurve < 0)
                     endcurve <- ifelse(length(endcurve), min(endcurve) - 
                       1, 105)
                     ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
                     maxcurve <- xcurve * xcurve + ycurve * ycurve
                     startcurve <- which(maxcurve > maxsd * maxsd)
                     startcurve <- ifelse(length(startcurve), max(startcurve) + 
                       1, 0)
                     lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                       col = gamma.col)
                     if (xcurve[labelpos[gindex]] > 0) 
                       boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                         gamma[gindex], border = FALSE)
                   }
               }
               xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
               ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
               lines(xcurve, ycurve)
               bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
               medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
               smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
               segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                   maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                   0.97 * maxsd)
               par(xpd = TRUE)
               if (obs.sd) {
                   xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
                   ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
                   lines(xcurve, ycurve)
               }
               points(sd.r, 0, cex = pcex)
               text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
                   1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                   0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                   0.95, 0.99), cex=cex.axis)
               text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315, cex=1.3)
               segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                   maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                   0.98 * maxsd)
               segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                   maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                   0.99 * maxsd)
           }
           else {
               x <- obs
               y <- sim
               R <- cor(x, y, use = "pairwise.complete.obs")
               E <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
               xprime <- x - mean(x, na.rm = TRUE)
               yprime <- y - mean(y, na.rm = TRUE)
               sumofsquares <- (xprime - yprime)^2
               Eprime <- sqrt(sum(sumofsquares)/length(complete.cases(x)))
               E2 <- E^2 + Eprime^2
               if (add == FALSE) {
                   maxray <- 1.5 * max(sd.f, sd.r)
                   plot(c(-maxray, maxray), c(0, maxray), type = "n", 
                     asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
                     xlab = xlab, ylab = ylab, main = main, cex = cex.axis)
                   discrete <- seq(180, 0, by = -1)
                   listepoints <- NULL
                   for (i in discrete) {
                     listepoints <- cbind(listepoints, maxray * 
                       cos(i * pi/180), maxray * sin(i * pi/180))
                   }
                   listepoints <- matrix(listepoints, 2, length(listepoints)/2)
                   listepoints <- t(listepoints)
                   lines(listepoints[, 1], listepoints[, 2])
                   lines(c(-maxray, maxray), c(0, 0))
                   lines(c(0, 0), c(0, maxray))
                   for (i in grad.corr.lines) {
                     lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                       i^2)), lty = 3)
                     lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                       i^2)), lty = 3)
                   }
                   for (i in grad.corr.full) {
                     text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                       i^2), i, cex = 0.6)
                     text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                       i^2), -i, cex = 0.6)
                   }
                   seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
                   for (i in seq.sd) {
                     xcircle <- sd.r + (cos(discrete * pi/180) * 
                       i)
                     ycircle <- sin(discrete * pi/180) * i
                     for (j in 1:length(xcircle)) {
                       if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
                         points(xcircle[j], ycircle[j], col = "darkgreen", 
                           pch = ".")
                         if (j == 10) 
                           text(xcircle[j], ycircle[j], signif(i, 
                             2), cex = 0.5, col = "darkgreen")
                       }
                     }
                   }
                   seq.sd <- seq.int(0, maxray, length.out = 5)
                   for (i in seq.sd) {
                     xcircle <- (cos(discrete * pi/180) * i)
                     ycircle <- sin(discrete * pi/180) * i
                     if (i) 
                       lines(xcircle, ycircle, lty = 3, col = "blue")
                     text(min(xcircle), -0.03 * maxray, signif(i, 
                       2), cex = 0.5, col = "blue")
                     text(max(xcircle), -0.03 * maxray, signif(i, 
                       2), cex = 0.5, col = "blue")
                   }
                   text(0, -0.08 * maxray, "Standard Deviation", 
                     cex = 0.7, col = "blue")
                   text(0, -0.12 * maxray, "Centered RMS Difference", 
                     cex = 0.7, col = "darkgreen")
                   points(sd.r, 0, pch = 22, bg = "darkgreen", cex = 1.1)
                   text(0, 1.1 * maxray, "Correlation", 
                     cex = 0.7)
               }
               S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
           }
       }
       points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, 
           cex = pcex)
       if (!is.null(text)) text(sd.f * R, sd.f * sin(acos(R)), labels=text, pos=3, col=col)
       invisible(oldpar)
   } # end taylor.diagram


   gr <- unique(groups)
   ngroups <- length(gr)
   if (is.null(col)) col <- piratepal("basel")[1:ngroups]
   if (is.null(text.groups)) text.groups <- gr
   
   # compute (normalized) standard deviation
   sd.obs.all <- sd(obs, na.rm=TRUE)
   sd.sim <- aggregate(sim, list(groups), sd, na.rm=TRUE)
   sd.obs <- aggregate(obs, list(groups), sd, na.rm=TRUE)
   if (normalize) {
      sd.sim$x <- sd.sim$x / sd.obs$x
      sd.obs.all <- 1
   }
   sd.sim$x[is.infinite(sd.sim$x)] <- NA
   first <- which.max(sd.sim$x)
   
   # compute correlation to check if plot should be produced with negative correlations
   if (is.null(pos.cor)) {
      r <- rep(NA, ngroups)
      for (i in 1:ngroups) {
            b <- groups == gr[i]
            obsmod <- na.omit(cbind(obs[b], sim[b])) 
            r[i] <- cor(obsmod[,1], obsmod[,2]) 
      }
      pos.cor <- TRUE
      if (any(r < 0)) pos.cor <- FALSE
   }
   
   b <- groups == gr[first]
   obsmod <- na.omit(cbind(obs[b], sim[b]))
   p <- .taylor.diagram(obsmod[,1], obsmod[,2], col=col[first], normalize=normalize, sd.arcs=sd.arcs, text=text.groups[first], pos.cor=pos.cor, ...)
   for (i in 1:ngroups) {
      if (i != first) {
         b <- groups == gr[i]
         obsmod <- na.omit(cbind(obs[b], sim[b]))
         .taylor.diagram(obsmod[,1], obsmod[,2], col=col[i], normalize=normalize, add=TRUE, text=text.groups[i], pos.cor=pos.cor, ...)
      }
   }
   if (plot.combined) .taylor.diagram(obs, sim, col="black", normalize=normalize, add=TRUE, text=text.combined, pos.cor=pos.cor, ...)
   text(sd.obs.all, 0, text.obs, pos=3)
   
}, ex=function() {

obs <- rnorm(50, 0, 5)
model1 <- obs + c(rnorm(25, 1, 2), rnorm(25, 4, 0.2))
model2 <- obs + c(rnorm(25, -5, 5), rnorm(25, 4, 0.2))
model3 <- obs + c(rnorm(25, 10, 10), rnorm(25, 4, 0.2))
data <- data.frame(obs=rep(obs, 3), model=c(model1, model2, model3), 
   group.models=rep(c("model1", "model2", "model3"), each=50), 
   group.models.subsets=rep(c("model1.subset1", "model1.subset2", 
   "model2.subset1", "model2.subset2", "model3.subset1", "model3.subset2"), each=25))

TaylorPlot(data$model, data$obs, data$group.models)
TaylorPlot(data$model, data$obs, data$group.models.subsets)


})

