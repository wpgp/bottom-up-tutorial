library(tidyverse)

n_obs <- 1000
prop_wider <- 0.2
poisson_rate_narrow <- rlnorm(round(n_obs*(1-prop_wider)), log(600), 0.15)
poisson_rate_wider <- rlnorm(round(n_obs*prop_wider), log(400), 0.12)
poisson_rate <- c(poisson_rate_narrow, poisson_rate_wider)
hist(poisson_rate)
observations <- tibble(
  pop = sapply(poisson_rate, function(x) rpois(1, x)),
  settlement = factor(c(rep(1, length(poisson_rate_narrow)), 
                 rep(2, length(poisson_rate_wider))))
) %>% 
  arrange(pop)
hist(observations$pop)

