#' Print all links of a given roadnet
#'
#' \code{all_link} receives a roadnet matrix and print all links with sep '-'.
#'
#' @param roadnet A weighted-adjacent-matrix of a roadnet. Every element represents the
#'   length of link identified by 2 points. The rownames and colnames of roadnet matrix
#'   should also be given, which is the point index/name.
#' @return A charactor vector contains all links in the roadnet.
#' @examples
#' A = matrix(c(0,2,6,0,2,0,5,7,6,5,0,1,0,7,1,0),nrow=4)
#' rownames(A) <- 11:14
#' colnames(A) <- 11:14
#' all_link(A)
#'
#' @export
#'
all_link <- function(roadnet) {
    x <- roadnet
    k <- list()
    for (i in 1:nrow(x)) {
        k[[i]] <- paste(rownames(x)[i], colnames(x)[which(x[i, ] != 0)], sep = "-")
    }
    k <- unlist(k)
    return(k)
}
