## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimEngine)

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_levels(n = c(200,400,800))

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_levels(
  n = c(10,100),
  distribution = list(
    "Beta 1" = list(type="Beta", params=c(0.3, 0.7)),
    "Beta 2" = list(type="Beta", params=c(1.5, 0.4)),
    "Normal" = list(type="Normal", params=c(3.0, 0.2))
  )
)
create_data <- function(n, type, params) {
  if (type=="Beta") {
    return(rbeta(n, shape1=params[1], shape2=params[2]))
  } else if (type=="Normal") {
    return(rnorm(n, mean=params[1], sd=params[2]))
  }
}
sim %<>% set_script(function() {
  x <- create_data(L$n, L$distribution$type, L$distribution$params)
  return(list("y"=mean(x)))
})
sim %<>% run()

## -----------------------------------------------------------------------------
sim %>% summarize(list(stat="mean", x="y"))

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_levels(alpha=c(2,3,4), beta=c(50,60))
print(sim$levels_grid)
sim %<>% set_levels(.keep=c(1,2,6))
print(sim$levels_grid)

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_levels(n=c(10, 100, 1000))
create_data <- function(n) {
  x <- runif(n)
  y <- 3 + 2*x + rnorm(n)
  return(data.frame("x"=x, "y"=y))
}
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  model <- lm(y~x, data=dat)
  return(list(
    "beta0_hat" = model$coefficients[[1]],
    "beta1_hat" = model$coefficients[[2]],
    ".complex" = list(
      "model" = model,
      "cov_mtx" = vcov(model)
    )
  ))
})
sim %<>% run()

## -----------------------------------------------------------------------------
head(sim$results)

## -----------------------------------------------------------------------------
c5 <- get_complex(sim, sim_uid=5)
print(summary(c5$model))
print(c5$cov_mtx)

## -----------------------------------------------------------------------------
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3)
sim %<>% set_script(function() {
  dat <- create_data(n=100)
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()

## -----------------------------------------------------------------------------
sim$results[order(sim$results$rep_id),c(1:7)!=5]

## -----------------------------------------------------------------------------
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3, batch_levels=NULL)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=100)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()

## -----------------------------------------------------------------------------
sim$results[order(sim$results$rep_id),c(1:7)!=5]

## -----------------------------------------------------------------------------
sim <- new_sim()
create_data <- function(n, mu) { rnorm(n=n, mean=mu) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(n=c(10,100), mu=c(3,5), est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=2, batch_levels=c("n", "mu"), return_batch_id=T)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=L$n, mu=L$mu)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()

sim$results[order(sim$results$batch_id),c(1:10)!=8]

## -----------------------------------------------------------------------------
sim %<>% set_config(seed=123)

## -----------------------------------------------------------------------------
sim <- new_sim()
print(vars(sim, "seed"))

## -----------------------------------------------------------------------------
sim <- new_sim()
sim %<>% set_config(num_sim=2)
sim %<>% set_levels(
  Sigma = list(
    s1 = list(mtx=matrix(c(3,1,1,2), nrow=2)),
    s3 = list(mtx=matrix(c(4,3,3,9), nrow=2)),
    s2 = list(mtx=matrix(c(1,2,2,1), nrow=2)),
    s4 = list(mtx=matrix(c(8,2,2,6), nrow=2))
  )
)
sim %<>% set_script(function() {
  x <- MASS::mvrnorm(n=1, mu=c(0,0), Sigma=L$Sigma$mtx)
  return(list(x1=x[1], x2=x[2]))
})
sim %<>% run()
print(sim$errors)

## -----------------------------------------------------------------------------
sim %<>% set_config(stop_at_error=TRUE)

## -----------------------------------------------------------------------------
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat) {
  return(mean(dat))
}
sim %<>% set_levels(n=c(10,100,1000))
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summarize(
  list(stat="mse", name="lambda_mse", estimate="lambda_hat", truth=5), 
  mc_se = TRUE
)

