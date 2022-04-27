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
# Code up the dataset-generating function
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

# Test the function
create_rct_data(5)

## -----------------------------------------------------------------------------
# Code up the estimators
est_tx_effect <- function(df, type) {
  n <- nrow(df)
  sum_t <- sum(df$outcome * (df$group=="treatment"))
  sum_c <- sum(df$outcome * (df$group=="control"))
  if (type=="est1") {
    true_prob <- 0.5
    return ( sum_t/(n*true_prob) - sum_c/(n*(1-true_prob)) )
  } else if (type=="est2") {
    est_prob <- sum(df$group=="treatment") / n
    return ( sum_t/(n*est_prob) - sum_c/(n*(1-est_prob)) )
  }
}

# Test out the estimators
df <- create_rct_data(10000)
est_tx_effect(df, "est1")
est_tx_effect(df, "est2")

## -----------------------------------------------------------------------------
sim %<>% set_levels(
  estimator = c("est1", "est2"),
  num_patients = c(50, 200, 1000)
)

## -----------------------------------------------------------------------------
sim %<>% set_script(function() {
  df <- create_rct_data(L$num_patients)
  est <- est_tx_effect(df, L$estimator)
  return (list("est"=est))
})

## -----------------------------------------------------------------------------
sim %<>% set_config(
  num_sim = 10,
  n_cores = 2,
  parallel = "outer",
  packages = c("ggplot2", "stringr")
)

## -----------------------------------------------------------------------------
sim %<>% run()

## -----------------------------------------------------------------------------
sim %>% summarize(
  bias = list(truth=-7, estimate="est"),
  mse = list(truth=-7, estimate="est")
)

## -----------------------------------------------------------------------------
head(sim$results)

