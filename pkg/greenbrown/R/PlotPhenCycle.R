PlotPhenCycle <- structure(function(
	##title<< 
	## Plot a easonal cycle with phenology metrics
	##description<<
	## This function plots a seasonal cycle with phenology metrics.
	
	x, 
	### values of one year
	
	xpred = NULL,
	### smoothed/predicted values 
		
	metrics,
	### vector of pheology metrics
	
	xlab="DOY",
	### label for x-axis
	
	ylab="NDVI",
	### label for y-axis
	
	trs = NULL,
	### threshold for threshold methods
	
	main="",
	### title
	
	...
	### further arguments (currently not used)
			
	##seealso<<
	## \code{\link{Phenology}}

) {
	
	sos <- metrics[1]
	eos <- metrics[2]
	los <- metrics[3]
	pop <- metrics[4]
	pot <- metrics[5]
	mgs <- metrics[6]
	rsp <- metrics[7]
	rau <- metrics[8]
	peak <- metrics[9]
	trough <- metrics[10]
	msp <- metrics[11]
	mau <- metrics[12]

	cols <- c("blue", "darkgreen", "orange")
	ylim <- range(c(x, xpred), na.rm=TRUE)
	ylim[2] <- ylim[2] + diff(ylim) * 0.1
	
	# plot time series
	t <- 1:length(x)
	plot(t, x, xlab=xlab, ylab=ylab, type="l", col="darkgrey", ylim=ylim, main=main, ...)
	if (!is.null(xpred)) lines(1:length(x), xpred, col="red")
	
	# plot SOS, EOS
	abline(v=c(sos, eos), col=cols[1:2], lty=2)
	text(x=c(sos, eos), y=min(ylim), paste(c("SOS", "EOS"), "=", round(c(sos, eos), 0)), col=cols[1:2], pos=3) 
	
	# plot POP
	abline(v=pop, col=cols[3], lty=2)
	text(x=pop, y=min(ylim)+diff(ylim)*0.1, paste("POP", "=", round(pop, 0)), col=cols[3], pos=3) 
	text(x=pop, y=max(ylim), paste("PEAK", "=", signif(peak, 2)), pos=1)

	# plot POT
	abline(v=pot, col=cols[3], lty=2)
	text(x=pot, y=min(ylim)+diff(ylim)*0.1, paste("POT", "=", round(pot, 0)), col=cols[3], pos=3) 
	text(x=pot, y=trough, paste("TROUGH", "=", signif(trough, 2)), pos=3)
	
	# # plot treshold
	# if (!is.null(trs)) {
		# abline(h=trs, lty=2)
		# text(x=1, y=trs, paste("trs =", signif(trs, 2)), pos=4)
	# }
	
	# plot LOS
	text(x=pop, y=mgs, paste(c("LOS"), "=", round(los, 0)), pos=1, col=cols[3]) 
	if (!is.na(eos) & !is.na(sos)) {
		if (sos < eos) segments(x0=sos, y0=mgs, x1=eos, y1=mgs, col=cols[3])
		if (sos > eos) segments(x0=c(0, sos), y0=c(mgs, mgs), x1=c(eos, length(t)), y1=c(mgs, mgs), col=cols[3])
	}
	
	# plot MSP and MAU averages
	segments(x0=c(sos-10, eos-10), y0=c(msp, mau), x1=c(sos+10, eos+10), y1=c(msp, mau), col=cols[1:2])	
	text(x=c(sos, eos, pop), y=c(msp, mau, mgs), paste(c("MSP", "MAU", "MGS"), "=", signif(c(msp, mau, mgs), 2)), pos=3, col=cols)
	
	# plot spring and autumn rates
	if (!is.na(rsp) | !is.na(rau)) {
		text(x=c(sos, eos), y=c(msp, mau), paste(c("RSP", "RAU"), "=", signif(c(rsp, rau), 2)), col=cols, pos=1) 
	}

}, ex=function() {

data(ndvi)
plot(ndvi)

# perform time series preprocessing for first year of data
x <- TsPP(ndvi, interpolate=TRUE)[1:365]
plot(x)

# calculate phenology metrics for first year
metrics <- PhenoTrs(x, approach="White")
PlotPhenCycle(x, metrics=metrics)

})




