mkGrid <- function(x, y, diam){
  res <- diam/5
  x0 <- x - 4 * res / 2
  y0 <- y - 4 * res / 2
  return(data.frame(x0, y0, res))
}
cat("\"mkGrid\" has load\n")