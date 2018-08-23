caculate_natureflow <- function(objlink, sflinkcol) {
    t <- where(function(x) objlink %in% x, sflinkcol)
    return(sum(t))
}

where <- function(f, x) {
    vapply(x, f, FUN.VALUE = logical(1))
}

#' @export
caculate_natureflow_by <- function(sfdf, bywhat) {
    fac <- as.factor(sfdf[[bywhat]])
    splitsfdf <- split(sfdf, fac)
    y <- lapply(splitsfdf, caculate_flow_cluster)
    z <- lapply(y, matrix)
    z <- as.data.frame(z)
    names(z) <- names(y)
    d <- data.frame(seqlink = alllink)
    d$seqlink <- as.character(d$seqlink)
    d$indexlink <- seqlinktoindexlink(d$seqlink)
    d <- d[, c(2, 1)]
    z <- cbind(d, z)
    return(z)
}

#' @export
caculate_natureflow_all <- function(sfdf) {
    l <- caculate_flow_cluster(sfdf)
    names(l) <- alllink
    d <- data.frame(traffic_volumn = simplify2array(l))
    d$seqlink <- rownames(d)
    d$indexlink <- seqlinktoindexlink(rownames(d))
    rownames(d) <- 1:nrow(d)
    d <- d[, c(3, 2, 1)]
    return(d)
}

caculate_flow_cluster <- function(df) {
    cores <- parallel::detectCores()
    cluster <- parallel::makePSOCKcluster(cores)
    outcome <- parallel::parLapply(cluster, alllink, caculate_natureflow, df$link)
    parallel::stopCluster(cluster)
    return(outcome)
}
