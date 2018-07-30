#' @export
caculate_natureflow <- function(sflinkcol,objlink){
  t <- lapply(sflinkcol,str_subset,objlink) %>% as.character()
  l <- length(t[t!="character(0)"])
  return(l)
}
