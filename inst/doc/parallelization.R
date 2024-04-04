## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimEngine)

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_config(parallel = TRUE)

## -----------------------------------------------------------------------------
sim %<>% set_config(n_cores = 2)

