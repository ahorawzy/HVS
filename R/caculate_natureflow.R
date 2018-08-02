#' @export
caculate_natureflow <- function(objlink,sflinkcol){
  t <- where(function(x) objlink %in% x, sflinkcol)
  return(sum(t))
}

where <- function(f, x){
  vapply(x, f, FUN.VALUE = logical(1))
}
