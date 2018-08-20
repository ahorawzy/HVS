## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(HVS)
data(road_net)
data(demosf)

## ------------------------------------------------------------------------
result <- dijkstra_matrix(road_net)
mileage <- result[[1]]
path <- result[[2]]

## ------------------------------------------------------------------------
demosf$path <- mapply(function(O,D) path[[O]][[D]],demosf$O,demosf$D)
head(demosf,10)

## ------------------------------------------------------------------------
demosf$link <- lapply(demosf$path,extract_link)
head(demosf,10)

## ------------------------------------------------------------------------
(all_link <- all_link(road_net))

## ------------------------------------------------------------------------
caculate_natureflow("1-2",demosf$link)

## ------------------------------------------------------------------------
sapply(all_link, caculate_natureflow,demosf$link)

