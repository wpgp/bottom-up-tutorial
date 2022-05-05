library(rstan)
library(tidyverse)
library(here)
library(plotly)

options(mc.cores = parallel::detectCores()-1)
rstan_options(auto_write = TRUE)


chains <- 4
iter <- 500
warmup <- 300
seed <- 12345677

observations <- read_csv(here('tutorials', 'refresher','observations.csv'),
                         col_types = "nf")

# Descriptive statistics --------------------------------------------------


hist(observations$pop)


# Fit a Poisson model -----------------------------------------------------

# Prior predictive check
alpha_national_simulated <- abs(rnorm(1000, 500, 50))
pop_poisson_simulated <- sapply(alpha_national_simulated, function(x) rpois(1, x))

hist(pop_poisson_simulated)

# fit the model
input_data <- list(
  n_obs =  nrow(observations),
  pop = observations$pop
)

pars_poisson <- c('alpha_national', 'pop_post_pred')

fit_poisson <- stan(
  file= here('tutorials', 'refresher','poisson.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_poisson
)

# extract model outputs
samples_poisson <- rstan::extract(fit_poisson)

# visualise convergence
traceplot(fit_poisson, pars = 'alpha_national')

# prior retrodective check
alpha_national <- tibble(
  posterior = samples_poisson$alpha_national,
  prior = abs(rnorm(chains*iter, 500, 50)),
  iter = 1:(chains*iter)
) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(alpha_national, aes(x=value, fill=distribution))+
  geom_histogram(binwidth = 5)+
  theme_minimal()

# posterior predictive check

pop_posterior <- tibble(
  source = factor(c(rep('predicted', iter*chains*input_data$n_obs), 
                    rep('observed', input_data$n_obs)), 
                  levels = c('predicted', 'observed')),
  value= c(as.vector(samples_poisson$pop_post_pred), input_data$pop)
)


ggplot(pop_posterior, aes(x=value, fill=source, after_stat(density)))+
  geom_histogram(binwidth=5, position='identity', alpha=0.7)+
  theme_minimal()



# Now your turn! ----------------------------------------------------------


