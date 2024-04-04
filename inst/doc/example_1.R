## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimEngine)

## -----------------------------------------------------------------------------
sim <- new_sim()

create_rct_data <- function(n, mu_0, mu_1, sigma_0, sigma_1) {
  group <- sample(rep(c(0,1),n))
  outcome <- (1-group) * rnorm(n=n, mean=mu_0, sd=sigma_0) +
             group * rnorm(n=n, mean=mu_1, sd=sigma_1)
  return(data.frame("group"=group, "outcome"=outcome))
}

# Test our data-generating function
create_rct_data(n=3, mu_0=3, mu_1=4, sigma_0=0.1, sigma_1=0.1)

## -----------------------------------------------------------------------------
run_test <- function(data) {
  test_result <- t.test(outcome~group, data=data)
  return(as.integer(test_result$p.value<0.05))
}

## -----------------------------------------------------------------------------
sim %<>% set_script(function() {
  data <- create_rct_data(n=L$n, mu_0=17, mu_1=18, sigma_0=2, sigma_1=2)
  reject <- run_test(data)
  return (list("reject"=reject))
})

sim %<>% set_levels(n=c(20,40,60,80))
sim %<>% set_config(num_sim=1000)

## -----------------------------------------------------------------------------
sim %<>% run()

power_sim <- sim %>% summarize(
  list(stat="mean", name="power", x="reject")
)
print(power_sim)

## ----message = FALSE, fig.width = 6-------------------------------------------
power_formula <- sapply(c(20,40,60,80), function(n) {
  pnorm(sqrt((n*(17-18)^2)/(2^2+2^2)) - qnorm(0.025, lower.tail=F))
})

library(ggplot2)
ggplot(data.frame(
  n = rep(c(20,40,60,80), 2),
  power = c(power_sim$power, power_formula),
  which = rep(c("Simulation","Formula"), each=4)
), aes(x=n, y=power, color=factor(which))) +
  geom_line() +
  labs(color="Method", y="Power", x="Sample size (per group)")

