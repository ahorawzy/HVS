#' Extract links from a path string.
#'
#' \code{extract_link} can extracts links information from a path string.
#'
#' @param path_i A string contains path information and seperated by ","
#' @return A character vector contains all links in this path.
#' @examples
#' extract_link("1,3,5,6,7,10")
#'
#' @export
#'
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
