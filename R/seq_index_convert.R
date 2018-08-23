#' @export
seqtoindex <- function(seqs) {
    return(indextable[seqs])
}

#' @export
indextoseq <- function(indexs) {
    return(vapply(indexs, findseq, numeric(1)))
}

findseq <- function(index) {
    if (index %in% indextable) {
        return(which(indextable == index))
    } else {
        return(0)
    }
}

#' @export
seqlinktoindexlink <- function(seqlinks) {
    return(vapply(seqlinks, findindexlink, character(1)))
}

findindexlink <- function(seqlink) {
    seqtwopoint <- as.numeric(strsplit(seqlink, split = "-")[[1]])
    indextwopoint <- seqtoindex(seqtwopoint)
    indexlink <- paste(indextwopoint, collapse = "-")
    return(indexlink)
}
