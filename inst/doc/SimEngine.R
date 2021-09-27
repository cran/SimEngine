## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SimEngine)

## -----------------------------------------------------------------------------
library(SimEngine)
sim <- new_sim()

## -----------------------------------------------------------------------------
create_rct_data <- function (num_patients) {
  df <- data.frame(
    "patient_id" = integer(),
    "group" = character(),
    "outcome" = double(),
    stringsAsFactors = FALSE
  )
  for (i in 1:num_patients) {
    group <- ifelse(sample(c(0,1), size=1)==1, "treatment", "control")
    treatment_effect <- ifelse(group=="treatment", -7, 0)
    outcome <- rnorm(n=1, mean=130, sd=5) + treatment_effect
    df[i,] <- list(i, group, outcome)
  }
  return (df)
}

# Test our creator function
create_rct_data(5)

## -----------------------------------------------------------------------------
sim %<>% add_creator(create_rct_data)

## -----------------------------------------------------------------------------
estimator_1 <- function(df) {
  n <- nrow(df)
  true_prob <- 0.5
  sum_t <- sum(df$outcome * (df$group=="treatment"))
  sum_c <- sum(df$outcome * (df$group=="control"))
  return ( sum_t/(n*true_prob) - sum_c/(n*(1-true_prob)) )
}
estimator_2 <- function(df) {
  n <- nrow(df)
  est_prob <- sum(df$group=="treatment") / n
  sum_t <- sum(df$outcome * (df$group=="treatment"))
  sum_c <- sum(df$outcome * (df$group=="control"))
  return ( sum_t/(n*est_prob) - sum_c/(n*(1-est_prob)) )
}

# Test our estimator functions
df <- create_rct_data(10000)
estimator_1(df)
estimator_2(df)

## -----------------------------------------------------------------------------
sim %<>% add_method(estimator_1)
sim %<>% add_method(estimator_2)

## -----------------------------------------------------------------------------
sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 200, 1000)
)

## -----------------------------------------------------------------------------
sim %<>% set_script(function() {
  df <- create_rct_data(L$num_patients)
  estimate <- use_method(L$estimator, list(df))
  return (list("estimate"=estimate))
})

## -----------------------------------------------------------------------------
sim %<>% set_config(
  num_sim = 10,
  n_cores = 2,
  parallel = "outer"
)

## -----------------------------------------------------------------------------
sim %<>% run()

## -----------------------------------------------------------------------------
sim %>% summarize(
  bias = list(name="bias_ate", truth=-7, estimate="estimate"),
  mse = list(name="mse_ate", truth=-7, estimate="estimate")
)

## -----------------------------------------------------------------------------
head(sim$results)

