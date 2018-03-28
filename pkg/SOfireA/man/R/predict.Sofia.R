predict.Sofia <- structure(function(
	##title<< 
	## Predict values based on a 'Sofia' object
	##description<<
	## Make a predicition based on a \code{\link{Sofia}} object and newdata
	
	object,
	### an object of class 'Sofia', see \code{\link{Sofia}}
	
	newdata,
	### a data frame with columns names as in object$group.names for area fractions of groups and as in object$x.names for explantory variables
	
	return.all=FALSE,
	### return all Sofia results? If FALSE, returns only total burned area
	
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{Sofia}}, \code{\link{SofiaOpt}}
) { 
   area <- newdata[, match(object$par$group.names, colnames(newdata))]
   x <- newdata[, match(object$x.names, colnames(newdata))]
   sf <- Sofia(x, area, per.group=object$per.group, sofiapar=object$par)
   if (return.all) {
      return(sf$data)
   } else {
      return(sf$data$y)
      ### A vector with predicted values.
   } 
   ### A vector with predicted values.
})
