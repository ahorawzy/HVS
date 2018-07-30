#' @export
extract_link <- function(path_i){
  p <- path_i
  p <- strsplit(p,split = ",")[[1]]
  Nlink <- length(p) - 1
  result <- numeric(length = Nlink)
  for(i in 1:Nlink){
    result[i] <- paste(p[i:(i+1)],collapse = "-")
  }
  return(result)
}
