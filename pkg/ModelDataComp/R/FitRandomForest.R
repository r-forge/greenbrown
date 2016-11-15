FitRandomForest <- structure(function(
	##title<< 
	## Fit a random forest to a data set
	##description<<
	## -
	
	x, 
	### predictor variables
	
	y, 
	### dependent variable
	
	nodes=4, 
	### number of nodes for parallel compuation of random forest
	
	ntree=500,
	### number of trees
	
	mtry=NULL,
	### Number of variables randomly sampled as candidates at each split. If NULL code{\link{tuneRF}} will be used to determine an optimal value for mtry
	
	xvars2 = NULL,
	
	...
	### further arguments

	##details<<
	## No details.
	
	##references<< No reference.	
	
	##seealso<<
	## \code{\link{FitDataModel}}
) { 

   require(randomForest)
   require(foreach)

   # tune mtry parameter of randomForest (= Number of variables randomly sampled as candidates at each split)
   if (is.null(mtry)) {
      message("RandomForest: Search best 'mtry' parameter.")
      mtry <- tuneRF(x=x, y=y, ntree=50, plot=TRUE, trace=TRUE)
      mtry.best <- mtry[which.min(mtry[,2]), 1]
      save(mtry, file="randomForest_tuneRF_mtry.RData")
      # mtry.best <- 48
   } else {
      mtry.best <- mtry
   }

   # setup for parallel computation
   parallel <- FALSE
   if (nodes > 1) parallel <- TRUE
   if (parallel) {
      message("RandomForest: Setup nodes for parallel computation.")
      cluster <- makeCluster(nodes)
      registerDoParallel(cluster, nodes)
   }

   # create random forest
   message("RandomForest: Train randomForest.")
   model <- foreach(ntr=rep(ntree/nodes, nodes), .combine=combine, .multicombine=TRUE, .packages="randomForest") %dopar% randomForest(x=x, y=y, importance=TRUE, mtry=mtry.best, ntree=ntr, do.trace=TRUE)
      
   # make specific plots
   setwd(path.exp)
   pdf("randomForest_varImpPlot.pdf", width=8, height=8)
   DefaultParL()
   imp <- varImpPlot(model, scale=TRUE)
   dev.off()
   write.table(imp, file="randomForest_varImpPlot_imp.txt")
   
   # select most important variables ...
   imp <- read.table("randomForest_varImpPlot_imp.txt")
   imp$r1 <- imp[,1] / max(imp[,1]) # rank IncMSE
   imp$r2 <- imp[,2] / max(imp[,2]) # rank IncNodePurity
   imp$r <- apply(cbind(imp$r1, imp$r2), 1, max) # combined rank
   imp.vars <- rownames(imp)[order(imp$r, decreasing=TRUE)]
   write.table(matrix(imp.vars, nrow=1), file="randomForest_important-variables.txt", row.names=FALSE, col.names=FALSE, sep=", ")

   if (parallel) stopCluster(cluster)
   return(model)
})

