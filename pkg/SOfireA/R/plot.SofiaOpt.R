plot.SofiaOpt <- structure(function(
	##title<< 
	## plot a SofiaOpt object
	##description<<
	## The optimization within \code{\link{SofiaFit}} produces files that can be used to restart or monitor an optimization experiment. These files can be read with \code{\link{ReadSofiaFit}} and plotted with this function..
	
	x,
	### an object of class \code{\link{SofiaFit}} as returned by \code{\link{ReadSofiaFit}}
	
	plot.objfct = c("Cor", "MEF", "Pbias"),
	### which objective function should be plotted (maximum 3)?
	
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{SofiaOpt}}
) { 

   # get cost
   cost <- unlist(llply(x, function(l) l$cost))
   o <- order(cost, decreasing=TRUE)
   isprior <- unlist(llply(x, function(l) all(l$par$prior == l$par$par)))[o]
   prior <- (1:length(cost))[isprior]
   
   par(mfrow=c(2,2))

   # plot development of cost
   mx <- max(cost, na.rm=TRUE)
   mn <- min(cost, na.rm=TRUE)
   ylim <- quantile(cost, c(0, 0.9), na.rm=TRUE)
   if (mx > (5 * mn)) ylim <- c(mn, mn*5)
   plot(cost[o], type="l", ylab="Cost", xlab="Iterations of genetic optimization", ylim=ylim)
   abline(v=prior, col="blue", lty=2)
   text(prior, mean(ylim), "prior", srt=90, pos=2, col="blue")
   text(1, ylim[2], paste("max =", signif(mx, 3)), pos=4, col="red")
   text(length(cost), ylim[1], paste("min =", signif(mn, 3)), pos=2, col="red")
   
   # plot other metrics
   obj.l <- llply(as.list(plot.objfct), function(objfct) {
      obj <- unlist(llply(x, function(l) l$obj[[objfct]]))
      ylim <- quantile(obj, c(0, 0.95), na.rm=TRUE)
      best <- obj[which.min(cost)]
      if (grepl("Cor", objfct) | grepl("MEF", objfct) | grepl("KGE", objfct) | grepl("IoA", objfct)) {
         ylim <- c(quantile(obj, 0.1, na.rm=TRUE), 1)
      }
      if (grepl("KGE.f", objfct)) ylim <- c(0, 1)
      plot(obj[o], type="l", ylab=objfct, ylim=ylim, xlab="Iterations of genetic optimization")
      abline(v=prior, col="blue", lty=2)
      text(prior, mean(ylim), "prior", srt=90, pos=2, col="blue")
      text(length(obj), best, paste(signif(best, 3)), pos=2, col="red")
   })
})
