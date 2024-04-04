## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(SimEngine)
sim <- new_sim()

## -----------------------------------------------------------------------------
create_data <- function(n) {
  return(rpois(n=n, lambda=20))
}

create_data(n=10)

## -----------------------------------------------------------------------------
est_lambda <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
dat <- create_data(n=1000)
est_lambda(dat=dat, type="M")
est_lambda(dat=dat, type="V")

## -----------------------------------------------------------------------------
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000)
)

## -----------------------------------------------------------------------------
sim %<>% set_script(function() {
  dat <- create_data(n=L$n)
  lambda_hat <- est_lambda(dat=dat, type=L$estimator)
  return (list("lambda_hat"=lambda_hat))
})

## -----------------------------------------------------------------------------
sim %<>% set_config(
  num_sim = 100,
  packages = c("ggplot2", "stringr")
)

## -----------------------------------------------------------------------------
sim %<>% run()

## -----------------------------------------------------------------------------
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)

## -----------------------------------------------------------------------------
head(sim$results)

## -----------------------------------------------------------------------------
sim %<>% set_config(num_sim = 200)
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000, 10000)
)

## -----------------------------------------------------------------------------
sim %<>% update_sim()

## -----------------------------------------------------------------------------
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)

