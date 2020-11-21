Col2 <- structure(function(
  ##title<< 
  ## Adds transparency to a colour
  ##description<< 
  ## The function adds transparency to a vector of colours
  
  col,
  ### character string of the colour
  
  alpha = 0.3,
  ### transparence (0 = transparent, 1 = opaque)
  
  ... 
  ### further arguments (unused)
  
  ##details<< 
  ## The function adds transparency to a vector of colours
  
  ##seealso<< 
  ## 
) {
  col2 <- col2rgb(col) / 255
  col2 <- rgb(col2[1,], col2[2,], col2[3,], alpha)
  return(col2)
  ### returns a colour including transparency
}, ex=function() {
  cols <- Col2(c("red", "blue"), alpha=seq(0, 1, 0.1))
  plot.new()
  legend("top", cols, fill=cols, bg="yellow")
})
