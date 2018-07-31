#' @export
all_link <- function(roadnet){
  x <- roadnet
  k <- list()
  for(i in 1:nrow(x)){
    k[[i]] <- paste(rownames(x)[i], which(x[i,]!=0),sep = "-")
  }
  k <- unlist(k)
  return(k)
}
