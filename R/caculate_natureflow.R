#' @export
caculate_natureflow <- function(sflinkcol,objlink){
  t <- as.character(lapply(sflinkcol,stringr::str_subset,objlink))
  l <- length(t[t!="character(0)"])
  return(l)
}
