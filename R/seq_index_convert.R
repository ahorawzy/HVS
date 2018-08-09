#' @export
seqtoindex <- function(seqs){
  return(indextable[seqs])
}

#' @export
indextoseq <- function(indexs){
  return(vapply(indexs, findseq, numeric(1)))
}

findseq <- function(index){
  if(index %in% indextable){
    return(which(indextable == index))
  } else {
    return(0)
  }
}
