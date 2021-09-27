# Misc
if (F) {

  sim <- new_sim()
  sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
  sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
  sim %<>% add_method("estimator_2", function(dat) { var(dat) })
  sim %<>% set_levels(
    "n" = c(10, 100, 1000),
    "estimator" = c("estimator_1", "estimator_2")
  )
  sim %<>% set_config(num_sim=1)
  sim %<>% set_script(function() {
    dat <- create_data(L$n)
    lambda_hat <- use_method(L$estimator, list(dat))
    return (list("lambda_hat"=lambda_hat))
  })


  sim %<>% run()
  suppressMessages(sim %<>% run())

  # Also do in parallel

  sim$results %>% print()


}

# Vignette scratch
if (F) {

  sim <- new_sim()
  sim %<>% set_levels(n=c(10, 100, 1000))
  sim %<>% add_creator("create_data", function(n) {
    x <- runif(n)
    y <- 3 + 2*x + rnorm(n)
    return(data.frame("x"=x, "y"=y))
  })
  sim %<>% set_config(num_sim=2)
  sim %<>% set_script(function() {
    dat <- create_data(L$n)
    model <- lm(y~x, data=dat)
    return (list(
      "beta0_hat" = model$coefficients[[1]],
      "beta1_hat" = model$coefficients[[2]],
      ".complex" = list(
        "model" = model,
        "cov_mtx" = vcov(model)
      )
    ))
  })
  sim %<>% run()

  # ...
  print(sim$results)

  # ...
  c5 <- get_complex(sim, sim_uid=5)
  print(summary(c5$model))


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
  sim %<>% add_creator("create_data", function(n, shape, rate) {
    return(rgamma(n, shape, rate))
  })
  sim %<>% add_method("kde_estimator", function(kernel, bandwidth) { ... })
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
