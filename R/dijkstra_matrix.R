#' @export
dijkstra_matrix <- function(weight_mat){
  A = weight_mat
  N = nrow(A)
  route_mileage <- matrix(-1,N,N)
  route_path <- list()

  for(i in 1:N){
    t <- dijkstra_i_path(A,i)
    route_mileage[i,] <- t[[1]]
    route_path[[i]] <- t[[2]]
  }

  return(list(route_mileage,route_path))
}
