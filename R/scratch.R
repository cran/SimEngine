
# Scratch
if (F) {

  library(tidyr)
  library(SimEngine)
  sim <- new_sim()

  create_regression_data <- function(n) {
    beta <- c(-1, 10)
    x <- rnorm(n)
    sigma2 <- exp(x)
    y <- rnorm(n=n, mean=(beta[1]+beta[2]*x), sd = sqrt(sigma2))
    return(data.frame(x=x, y=y))
  }
  dat <- create_regression_data(n=500)
  linear_model <- lm(y~x, data=dat)
  dat$residuals <- linear_model$residuals

  library(ggplot2)
  ggplot(dat, aes(x=x, y=residuals)) +
    geom_point() +
    theme_bw() +
    labs(x="x", y="residual")
  model_vcov <- function(data) {
    mod <- lm(y~x, data=data)
    return(list("coef"=mod$coefficients, "vcov"=diag(vcov(mod))))
  }

  sandwich_vcov <- function(data) {
    mod <- lm(y~x, data=data)
    return(list("coef"=mod$coefficients, "vcov"=diag(vcovHC(mod))))
  }

  sim %<>% set_script(function() {
    data <- create_regression_data(n=L$n)
    estimates <- use_method(L$estimator, list(data))
    return(list(
      "beta0_est" = estimates$coef[1],
      "beta1_est" = estimates$coef[2],
      "beta0_se_est" = sqrt(estimates$vcov[1]),
      "beta1_se_est" = sqrt(estimates$vcov[2])
    ))
  })

  sim %<>% set_levels(
    estimator = c("model_vcov", "sandwich_vcov"),
    n = c(50, 100, 500, 1000)
  )

  sim %<>% set_config(
    num_sim = 1,
    seed = 24,
    packages = c("sandwich")
  )

  sim %<>% run()

  summarized_results <- sim %>% summarize(
    mean = list(
      list(name="mean_se_beta0", x="beta0_se_est"),
      list(name="mean_se_beta1", x="beta1_se_est")
    ),
    coverage = list(
      list(name="cov_beta0", estimate="beta0_est", se="beta0_se_est", truth=-1),
      list(name="cov_beta1", estimate="beta1_est", se="beta1_se_est", truth=10)
    )
  )

  print(summarized_results)

  bootstrap_vcov <- function(data) {
    mod <- lm(y~x, data=data)
    boot_ests <- matrix(NA, nrow=100, ncol=2)
    for (j in 1:100) {
      indices <- sample(1:nrow(data), size=nrow(data), replace=TRUE)
      boot_dat <- data[indices,]
      boot_mod <- lm(y~x, data=boot_dat)
      boot_ests[j,] <- boot_mod$coefficients
    }
    boot_v1 <- var(boot_ests[,1])
    boot_v2 <- var(boot_ests[,2])
    return(list("coef"=mod$coefficients, "vcov"=c(boot_v1, boot_v2)))
  }

  sim %<>% set_levels(
    estimator = c("model_vcov", "sandwich_vcov", "bootstrap_vcov"),
    n = c(50, 100, 500, 1000)
  )

  sim %<>% set_config(
    num_sim = 1,
    seed = 24,
    parallel = "none",
    n_cores = 2,
    packages = c("sandwich")
  )

  sim %<>% update_sim()

}

# Working on system for restricted level sets
if (F) {

  library(SimEngine)
  sim <- new_sim()
  sim %<>% set_config(num_sim=2)
  sim %<>% set_levels(
    n = c(100,400),
    method = list(
      "mth 1" = list(est="kernel", bnd=0.1),
      "mth 2" = list(est="lasso", bnd=0.2)
    )
  )

}

# Fixing summarize() naming issues
if (F) {

  d <- data.frame(grp=c(1,1,2,2,3,3), `y(0)`=c(11,22,33,44,55,66))
  # Do a naming check and throw an error if it's not a valid dataframe col name
  d
  d %>% group_by(grp) %>% summarize(
    "mean_y(0)" = mean(y),
    `sum_y(0)` = sum(y)
  )

}

# Rlecuyer package
if (F) {

  # generate 20 random numbers from 10 Lecuyer's streams
  library(rlecuyer)

  system.time({

    nstreams <- 40000  # number of streams
    n <- 20 # number of RNs per stream

    # set seed for the whole package of streams
    .lec.SetPackageSeed(1234)

    stream.names <- paste0("mystream",1:nstreams) # stream names
    .lec.CreateStream(stream.names) # initializes given set of streams

  })

  RNs <- NULL
  tid <- 123
  .lec.CurrentStream(stream.names[tid]) # switch to the i-th stream
  RNs <- rbind(RNs, runif(n)) # generate random numbers from the given stream
  .lec.CurrentStreamEnd() # unset the current stream

  print(RNs)

  # delete existing streams
  .lec.DeleteStream(stream.names)

}

# Github issue #27
# Need a system to allow multiple methods to be executed on a single dataset
# !!!!! Deprioritizing this for now
if (F) {
  sim <- new_sim()
  create_data <- function(n, shape, rate) {
    return(rgamma(n, shape, rate))
  }
  kde_estimator <- function(kernel, bandwidth) { ... }
  sim %<>% set_levels(
    "n" = c(20, 50),                        # creator param
    "shape" = c(2, 4),                      # creator param
    "rate" = c(3, 6),                       # creator param
    "kernel" = c("gaussian", "triangular"), # other param
    "bandwidth" = c(0.2, 0.8)               # other param
  )
  sim %<>% set_script(function() {
    sim_seed("creator")
    dat <- create_data(L$n, L$shape, L$rate)
    sim_seed("other")
    x <- rnorm(100)
    kde <- kde_estimator(x, L$kernel, L$bandwidth) # placeholder
    return (list("kde"=kde))
  })
  sim %<>% run()

  # Current operation
  sim %<>% set_config(datasets="many") # This gives normal operation
  sim %<>% set_config(
    datasets = "one",
    creator_levels = c("n", "shape", "rate")
  )

  # * use uids for seeds? with multiplier?
}
