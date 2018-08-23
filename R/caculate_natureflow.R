#' @export
caculate_natureflow <- function(objlink,sflinkcol){
  t <- where(function(x) objlink %in% x, sflinkcol)
  return(sum(t))
}

where <- function(f, x){
  vapply(x, f, FUN.VALUE = logical(1))
}

#' @export
caculate_natureflow_by <- function(sfdf,bywhat){
  fac <- as.factor(sfdf[[bywhat]])
  splitsfdf <- split(sfdf,fac)
  y <- lapply(splitsfdf, caculate_flow_cluster)
  z <- lapply(y, matrix)
  z <- as.data.frame(z)
  names(z) <- names(y)
  return(z)
}

caculate_flow_cluster <- function(df){
  cores <- parallel::detectCores()
  cluster <- parallel::makePSOCKcluster(cores)
  outcome <- parallel::parLapply(cluster,alllink,caculate_natureflow,df$link)
  parallel::stopCluster(cluster)
  return(outcome)
}
