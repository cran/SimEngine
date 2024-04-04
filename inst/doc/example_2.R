## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimEngine)

## -----------------------------------------------------------------------------
sim <- new_sim()

create_regression_data <- function(n) {
  beta <- c(-1, 10)
  x <- rnorm(n)
  sigma2 <- exp(x)
  y <- rnorm(n=n, mean=(beta[1]+beta[2]*x), sd = sqrt(sigma2))
  return(data.frame(x=x, y=y))
}

## ----message = FALSE, fig.width = 6-------------------------------------------
dat <- create_regression_data(n=500)
linear_model <- lm(y~x, data=dat)
dat$residuals <- linear_model$residuals

library(ggplot2)
ggplot(dat, aes(x=x, y=residuals)) +
  geom_point() +
  theme_bw() +
  labs(x="x", y="residual")

## -----------------------------------------------------------------------------
model_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcov(mod))))
}

sandwich_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcovHC(mod))))
}

## -----------------------------------------------------------------------------
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
  num_sim = 500,
  seed = 24,
  packages = c("sandwich")
)

sim %<>% run()

## -----------------------------------------------------------------------------
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)

print(summarized_results)

## -----------------------------------------------------------------------------
library(tidyr)
plot_results <- function(which_graph, n_est) {
  if (n_est == 3) {
    values <- c("#999999", "#E69F00", "#56B4E9")
    breaks <- c("model_vcov", "sandwich_vcov", "bootstrap_vcov")
    labels <- c("Model-based", "Sandwich", "Bootstrap")
  } else {
    values <- c("#999999", "#E69F00")
    breaks <- c("model_vcov", "sandwich_vcov")
    labels <- c("Model-based", "Sandwich")
  }
  if (which_graph == "width") {
    summarized_results %>%
    pivot_longer(
      cols = c("mean_se_beta0", "mean_se_beta1"),
      names_to = "parameter",
      names_prefix = "mean_se_"
    ) %>%
    dplyr::mutate(value_j = jitter(value, amount = 0.01)) %>%
    ggplot(aes(x=n, y=1.96*value_j, color=estimator)) +
    geom_line(aes(linetype=parameter)) +
    geom_point() +
    theme_bw() +
    ylab("Average CI width") +
    scale_color_manual(
      values = values,
      breaks = breaks,
      name = "SE estimator",
      labels = labels
    ) +
    scale_linetype_discrete(
      breaks = c("beta0", "beta1"),
      name = "Parameter",
      labels = c(expression(beta[0]), expression(beta[1]))
    )
  } else {
    summarized_results %>%
    pivot_longer(
      cols = c("cov_beta0","cov_beta1"),
      names_to = "parameter",
      names_prefix = "cov_"
    ) %>%
    dplyr::mutate(value_j = jitter(value, amount = 0.01)) %>%
    ggplot(aes(x=n, y=value, color=estimator)) +
    geom_line(aes(linetype = parameter)) +
    geom_point() +
    theme_bw() +
    ylab("Coverage") +
    scale_color_manual(
      values = values,
      breaks = breaks,
      name = "SE estimator",
      labels = labels
    ) +
    scale_linetype_discrete(
      breaks = c("beta0", "beta1"),
      name = "Parameter",
      labels = c(expression(beta[0]), expression(beta[1]))
    ) +
    geom_hline(yintercept=0.95)
  }
}

## ----message = FALSE, fig.width = 6-------------------------------------------
plot_results("width", 2)
plot_results("coverage", 2)

## -----------------------------------------------------------------------------
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
  num_sim = 500,
  seed = 24,
  parallel = TRUE,
  n_cores = 2,
  packages = c("sandwich")
)

sim %<>% update_sim()

## ----fig.width = 6------------------------------------------------------------
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)

plot_results("width", 3)
plot_results("coverage", 3)

