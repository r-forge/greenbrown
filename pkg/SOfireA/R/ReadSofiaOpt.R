ReadSofiaOpt <- structure(function(
	##title<< 
	## Read results from an SOFIA optimization experiment 
	##description<<
	## The optimization within \code{\link{SofiaOpt}} produces files that can be used to restart or monitor an optimization experiment. This function reads these files.
	
	files,
	### vector of file names
	
	combine = TRUE,
	### combine several files in a single file?
	
   ...
   ### further arguments (not used)

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{SofiaOpt}}
) { 
   load(files[1])
   result[[1]]$sofia$par <- NULL
   result[[1]]$sofia$data <- NULL
   result.l <- result
   if (length(files) > 1) {
      for (i in 2:length(files)) {
         x <- try(load(files[i]), silent=TRUE)
         if (class(x) == "try-error") return(NULL)
         result[[1]]$sofia$par <- NULL
         result[[1]]$sofia$data <- NULL
         result.l <- c(result.l, result)
      }
   } else {
      combine <- FALSE
   }
   result <- result.l
   
   # remove duplicates
   txt <- unlist(llply(result, function(l) paste(c(round(l$cost, 10), round(l$dpar, 6)), collapse=" ")))
   dup <- duplicated(txt)
   result <- result[!dup]
   
   # check cost
   cost <- unlist(llply(result, function(l) l$cost))
   result <- result[!is.na(cost)]
   
   # combine
   if (combine) {
      cost <- cost[!is.na(cost)]
      o <- order(cost, decreasing=TRUE)
      result <- result[o]
   
      file.remove(files)
      save(result, file=tempfile("genoud_", tmpdir=getwd(), "_SofiaOpt.RData"))
   }
   class(result) <- "SofiaOpt"
   return(result)
})
