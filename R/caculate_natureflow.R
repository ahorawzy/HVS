#' @export
caculate_natureflow <- function(objlink,sflinkcol){
  # t <- vapply(sflinkcol, function(x,y) y %in% x, y = objlink,logical(1))
  # return(sum(t))
  t <- Filter(function(x) objlink %in% x, sflinkcol)
  return(length(t))
}
