
# Simple example for batch()
if (F) {

  sim <- new_sim()
  create_data <- function(n) { rnorm(n, mean=3) }
  est_mean <- function(dat, type) {
    if (type=="est_mean") { return(mean(dat)) }
    if (type=="est_median") { return(median(dat)) }
  }
  sim %<>% set_levels(est=c("est_mean","est_median"))
  sim %<>% set_config(num_sim=3, batch_levels=NULL)
  sim %<>% set_script(function() {
    dat <- create_data(n=100)
    # batch({
    #   dat <- create_data(n=100)
    # })
    mu_hat <- est_mean(dat=dat, type=L$est)
    return(list(
      "mu_hat" = mu_hat,
      "dat1" = round(dat[1],2),
      "dat2" = round(dat[2],2),
      "dat3" = round(dat[3],2)
    ))
  })
  sim %<>% run()
  sim$results[order(sim$results$rep_id),]

}

# update copy
if (F) {

  # Scratch 3: profiling
  if (F) {

    sim <- new_sim()
    create_data <- function(n, mu) { rnorm(n, mean=mu, sd=0.5) }
    sim %<>% set_levels(n=c(10,100), mu=c(2,5), est=c("H","E","Y"))
    sim %<>% set_config(num_sim=3, batch_levels=c("n","mu"))
    sim %<>% set_script(function() {
      batch({ dat <- create_data(L$n, L$mu) })
      return (list("mean_dat" = mean(dat)))
    })
    sim %<>% run()

  }

  # Scratch 2 (using this)
  if (F) {

    sim <- new_sim()
    create_data <- function(n) { rpois(n, lambda=5) }
    est_mean <- function(dat, type) {
      if (type=="M") { return(mean(dat)) }
      if (type=="V") { return(var(dat)) }
    }
    sim %<>% set_levels(n=c(10,100), est="M")
    sim %<>% set_config(num_sim=5, n_cores=3, parallel="outer")
    sim %<>% set_script(function() {
      dat <- create_data(L$n)
      lambda_hat <- est_mean(dat=dat, type=L$est)
      return (list(
        "lambda_hat" = lambda_hat,
        .complex = list(a=1, b=lambda_hat)
      ))
    })
    sim %<>% run()
    sim %<>% set_levels(n=c(10,1000), est=c("M","V"))
    # sim %<>% set_levels(n=c(10,1000), est=c("M","V"), hey=2)
    sim %<>% set_config(num_sim=3)
    sim %<>% update_sim()

  }

  # Scratch 1
  if (F) {

    sim <- new_sim()
    create_data <- function(n) { rpois(n, lambda=5) }
    est_mean <- function(dat, type) {
      if (type=="M") { return(mean(dat)) }
      if (type=="V") { return(var(dat)) }
    }
    sim %<>% set_levels(
      n = c(10,100,1000),
      estimator = c("M","V")
      # estimator = list(
      #   "est 1" = list(a=1, b=2),
      #   "est 2" = list(a=3, b=44, c=55)
      # )
    )
    sim %<>% set_config(num_sim=2)
    sim %<>% set_script(function() {
      dat <- create_data(L$n)
      lambda_hat <- est_mean(dat=dat, type=L$est)
      return (list("lambda_hat"=lambda_hat))
    })
    sim %<>% run()



  }

}

# Testing batch() function: object created within batch()
if (F) {

  sim <- new_sim()
  create_data <- function(n, lmbd) { rpois(n, lambda=lmbd) }
  est_mean <- function(dat, type) {
    if (type=="M") { return(mean(dat)) }
    if (type=="V") { return(var(dat)) }
  }
  sim %<>% set_levels(n=c(10,100), lmbd=c(3,5), est=c("M","V"))
  # sim %<>% set_config(num_sim=2)
  sim %<>% set_config(num_sim=2, batch_levels=c("n", "lmbd"))
  sim %<>% set_script(function() {
    # dat <- create_data(L$n, L$lmbd)
    batch({
      dat <- create_data(L$n, L$lmbd)
    })
    lambda_hat <- est_mean(dat=dat, type=L$est)
    return (list(
      "lambda_hat" = lambda_hat,
      "mean" = mean(dat),
      ".complex" = list(a=1, m=mean(dat))
    ))
  })

  sim %<>% run()
  sim$results
  # sim$results %>% arrange(batch_id)

}

# New progress bar
if (F) {

  # Example from stack overflow
  n=1000
  df=data.frame(time=1:n,y=runif(n))
  window=100
  for(i in 1:(n-window)) {
    flush.console()
    plot(df$time,df$y,type='l',xlim=c(i,i+window))
    Sys.sleep(.09)
  }

  replaceMessage <- function(x, width = 80) {
    message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
    message(x, appendLF = F)
  }

  replaceMessage("Saving...\nSaving..."); Sys.sleep(1); replaceMessage("Saved\nSaved");

  #      |########################################| 100%

  for (i in 12:8) {
    message(paste0(i), "\r", appendLF=F)
    # message(paste0(i,"\n",i), "\r", appendLF=F)
    # message("\r", i, "\r")
    # flush.console()
    Sys.sleep(1)
  }
  #



  .finished <- F
  .count <- 1
  while (!.finished) {

    Sys.sleep(1)
    .count <- .count + 1
    if (.count>5) { .finished <- T }

  }


  for (i in 1:3) {
    cat(paste0(
      "Core 01: ", 123, "\n",
      "Core 02: ", 456, "\n",
      "Core 03: ", 789, "\n"
    ), "\r")
    flush.console()
    Sys.sleep(1)
  }

  for (i in 1:3) {
    cat(paste0(i,"\n"), " \r")
    cat(paste0(i,"\n"), " \r")
    # flush.console()
    Sys.sleep(1)
  }



}

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
