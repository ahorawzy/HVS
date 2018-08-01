#' @export
caculate_natureflow <- function(objlink,sflinkcol){
  t <- sapply(sflinkcol, function(x,y) y %in% x, y = objlink)
  return(sum(t))
}
