
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

# Fit a Poisson-Lognormal model -----------------------------------------------------

# Prior predictive check
alpha_simulated <- abs(rnorm(1000, log(500), 0.1))
sigma_simulated <- abs(rnorm(1000, 0.3, 0.1))
lambda_simulated <- mapply( function(x,y) rlnorm(1, x, y), alpha_simulated, sigma_simulated )
pop_lognormal_simulated <- sapply(lambda_simulated, function(x) rpois(1, x))
hist(pop_lognormal_simulated)

comp_pop <- tibble(
  model = factor(c(rep('Poisson', length(pop_poisson_simulated)), 
                   rep('Poisson-Lognormal',length(pop_lognormal_simulated))),
                 levels=c('Poisson-Lognormal','Poisson')),
  value= c(pop_poisson_simulated,pop_lognormal_simulated)
)

ggplot(comp_pop, aes(x=value, fill=model))+
  geom_histogram(bins = 100,position='identity', alpha=0.8)+
  theme_minimal()

# run the model
pars_lognormal <- c('alpha_national', 'sigma', 'pop_post_pred')

fit_poisson_lognormal <- stan(
  file= here('tutorials', 'refresher','poisson_lognormal.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal
)

traceplot(fit_poisson_lognormal, pars = 'alpha_national')
traceplot(fit_poisson_lognormal, pars = 'sigma')


samples_poisson_lognormal <- rstan::extract(fit_poisson_lognormal)

# prior retrodictive check
alpha_national <- tibble(
  posterior = samples_poisson_lognormal$alpha_national,
  prior = abs(rnorm(chains*iter, log(500), 0.1)),
  iter = 1:(chains*iter)
) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(alpha_national, aes(x=value, fill=distribution))+
  geom_histogram(bins=100, position='identity', alpha=0.8)+
  theme_minimal()

sigma <- tibble(
  posterior = samples_poisson_lognormal$sigma,
  prior = abs(rnorm(chains*iter, 0.3, 0.1)),
  iter = 1:(chains*iter)) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(sigma, aes(x=value, fill=distribution))+
  geom_histogram(bins=100, position='identity', alpha=0.8)+
  theme_minimal()

# posterior predictive check

pop_posterior_lognormal <- tibble(
  source = factor(c(rep('predicted_poisson', iter*chains*input_data$n_obs), 
                    rep('predicted_poisson_lognormal', iter*chains*input_data$n_obs),
                    rep('observed', input_data$n_obs)), 
                  levels = c('observed', 'predicted_poisson_lognormal', 'predicted_poisson')),
  value= c(
    as.vector(samples_poisson$pop_post_pred), 
    as.vector(samples_poisson_lognormal$pop_post_pred), 
    input_data$pop)
)


ggplot(pop_posterior_lognormal, aes(x=value, fill=source, after_stat(density)))+
  geom_histogram(bins=100, position='identity', alpha=0.7)+
  theme_minimal()


# Fit a random slope model  --------------------------------------------------

# Descriptive statistics 

ggplot(observations, aes(x=pop, fill=settlement, after_stat(density)))+
  geom_histogram(bins=50, position='identity', alpha=0.8)+
  theme_minimal()


# run the model
input_data <- list(
  n_obs =  nrow(observations),
  pop = observations$pop,
  n_settlement = nlevels(observations$settlement),
  obs_to_settlement = as.integer(observations$settlement)
)


pars_lognormal_independent <- c('pop_post_pred', 'alpha_settlement',  'sigma')


fit_poisson_lognormal_independent <- stan(
  file= here('tutorials', 'refresher','poisson_independent.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal_independent
)

traceplot(fit_poisson_lognormal_independent, pars = 'alpha_settlement')
traceplot(fit_poisson_lognormal_independent, pars = 'sigma')

samples_poisson_lognormal_independent <- rstan::extract(fit_poisson_lognormal_independent)


# prior retrodictive check
alpha_settlement <- as_tibble(samples_poisson_lognormal_independent$alpha_settlement)
colnames(alpha_settlement) <- c('posterior_alpha_settlement_1', 'posterior_alpha_settlement_2')

alpha_settlement <- alpha_settlement %>% 
  mutate (
    prior = abs(rnorm(chains*iter, log(500), 0.1)),
    iter = 1:(chains*iter)
  ) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(alpha_settlement, aes(x=value, fill=distribution))+
  geom_histogram(bins=100,position='identity', alpha=0.7)+
  theme_minimal()

# posterior predictive check

pop_posterior_lognormal_independent <- tibble(
  source = factor(c(
    rep('predicted_poisson_lognormal', iter*chains*input_data$n_obs),
    rep('predicted_poisson_lognormal_independent', iter*chains*input_data$n_obs),
    rep('observed', input_data$n_obs)), 
    levels = c('observed','predicted_poisson_lognormal_independent', 
               'predicted_poisson_lognormal')),
  value= c(
    as.vector(samples_poisson_lognormal$pop_post_pred), 
    as.vector(samples_poisson_lognormal_independent$pop_post_pred),
    input_data$pop)
)


ggplotly(ggplot(pop_posterior_lognormal_independent, aes(x=value, fill=source, after_stat(density)))+
           geom_histogram(bins=50, position='identity', alpha=0.5)+
           theme_minimal())



# Fit a hierarchical model ------------------------------------------------


pars_lognormal_hierarchical_cp <- c('alpha_national','alpha_settlement', 'u_alpha_settlement', 'sigma', 'pop_post_pred')

fit_poisson_lognormal_hierarchical_cp <- stan(
  file= here('tutorials', 'refresher','poisson_hierarchical_cp.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal_hierarchical_cp
)

traceplot(fit_poisson_lognormal_hierarchical_cp, pars = 'alpha_settlement')
traceplot(fit_poisson_lognormal_hierarchical_cp, pars = 'alpha_national')
traceplot(fit_poisson_lognormal_hierarchical_cp, pars = 'u_alpha_settlement')
traceplot(fit_poisson_lognormal_hierarchical_cp, pars = 'sigma')

samples_poisson_lognormal_hierarchical_cp<- rstan::extract(fit_poisson_lognormal_hierarchical_cp)


# prior retrodictive check
alpha <- as_tibble(samples_poisson_lognormal_hierarchical_cp$alpha_settlement)
colnames(alpha) <- c('posterior_alpha_settlement_1', 'posterior_alpha_settlement_2')

alpha <- alpha %>% 
  mutate (
    posterior_alpha_national = samples_poisson_lognormal_hierarchical_cp$alpha_national,
    prior = abs(rnorm(chains*iter, log(500), 0.1)),
    iter = 1:(chains*iter)
  ) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(alpha, aes(x=value, fill=distribution))+
  geom_histogram(bins=100,position='identity', alpha=0.7)+
  theme_minimal()

comp_alpha <- rbind(
  alpha %>% 
    mutate(
      distribution = paste0(distribution, '_hierarchical'),
      model= 'hierarchical'
    ),
  alpha_settlement %>% 
    mutate(
      distribution = paste0(distribution, '_independant'),
      model = 'independant'
    )
)  %>% 
  filter(!grepl('prior',distribution))


ggplot(comp_alpha, aes(x=value, y=distribution, fill=model))+
  geom_boxplot()+
  theme_minimal()+
  guides(fill='none')

comp_alpha %>% 
  group_by(distribution) %>% 
  summarise(
    mean(value),
    sd(value)
  )


pop_posterior_lognormal_hierarchical <- tibble(
  source = factor(c( 
    rep('predicted_poisson_lognormal_independent', iter*chains*input_data$n_obs),
    rep('predicted_poisson_lognormal_hierarchical', iter*chains*input_data$n_obs),
    rep('observed', input_data$n_obs)), 
    levels = c('observed',
               'predicted_poisson_lognormal_hierarchical',
               'predicted_poisson_lognormal_independent')),
  value= c(
    as.vector(samples_poisson_lognormal_independent$pop_post_pred),
    as.vector(samples_poisson_lognormal_hierarchical_cp$pop_post_pred),
    input_data$pop)
)


ggplotly(ggplot(pop_posterior_lognormal_hierarchical, aes(x=value, fill=source, after_stat(density)))+
           geom_histogram(bins=50, position='identity', alpha=0.5)+
           theme_minimal())


# New validation: cluster-based
comp_obs <- as_tibble(samples_poisson_lognormal_hierarchical_cp$pop_post_pred) %>% 
  summarise(across(
    everything(), ~ quantile(., probs=c(0.025, 0.5, 0.975))
  )) %>% 
  mutate(
    metrics = paste0('q', c(0.025, 0.5, 0.975))
  ) %>% 
  pivot_longer(
    -metrics
  ) %>% 
  pivot_wider(names_from = metrics, values_from = value) %>% 
  mutate(
    obs = input_data$pop
  )

ggplot(comp_obs, aes(x=obs, y=q0.5, ymin=q0.025, ymax=q0.975))+
  geom_pointrange()+
  theme_minimal()+
  labs(x='observations', y='predictions')+
  geom_abline(intercept=0, slope=1, size=1, color='orange')


# New parametrization: non-centering the location ---------------------------------


pars_lognormal_hierarchical_nclp <- c('alpha_national','delta_settlement', 'u_delta_settlement', 
                                      'sigma', 'pop_post_pred')

fit_poisson_lognormal_hierarchical_nclp <- stan(
  file= here('tutorials', 'refresher','poisson_hierarchical_nclp.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal_hierarchical_nclp
)

traceplot(fit_poisson_lognormal_hierarchical_nclp, pars = 'delta_settlement')
traceplot(fit_poisson_lognormal_hierarchical_nclp, pars = 'alpha_national')
traceplot(fit_poisson_lognormal_hierarchical_nclp, pars = 'u_delta_settlement')
traceplot(fit_poisson_lognormal_hierarchical_nclp, pars = 'sigma')


samples_poisson_lognormal_hierarchical_nclp<- rstan::extract(fit_poisson_lognormal_hierarchical_nclp)

# prior retrodictive check
mu <- as_tibble(samples_poisson_lognormal_hierarchical_nclp$delta_settlement)
colnames(mu) <- c('posterior_delta_settlement_1', 'posterior_delta_settlement_2')

mu <- mu %>% 
  mutate(
    posterior_baseline = samples_poisson_lognormal_hierarchical_nclp$alpha_national,
    posterior_mu_settlement_1 = posterior_baseline + posterior_delta_settlement_1,
    posterior_mu_settlement_2 = posterior_baseline + posterior_delta_settlement_2,
    iter = 1:(chains*iter)
  ) %>% 
  select(-posterior_delta_settlement_1, -posterior_delta_settlement_2) %>% 
  pivot_longer(-iter, names_to = 'posterior')

ggplot(mu, aes(x= posterior, y=value, fill=posterior))+
  geom_boxplot()+
  theme_minimal()+
  guides(fill='none')

comp_parametrization <- rbind(
  mu %>% 
    filter(
      posterior %in% c('posterior_mu_settlement_1','posterior_mu_settlement_2')
    ) %>% 
    mutate(
      parametrization = 'cp'
    ),
  alpha %>% 
    filter(
      distribution %in% c('posterior_alpha_settlement_1', 'posterior_alpha_settlement_2')
    ) %>% 
    rename(posterior=distribution) %>% 
    mutate(
      parametrization = 'nclp'
    )
)

ggplot(comp_parametrization, aes(x=value, fill=parametrization))+
  geom_histogram(bins=100, position='identity', alpha=0.7)+
  theme_minimal()

# posterior predictive check
pop_posterior_lognormal_hierarchical <- tibble(
  source = factor(c(rep('predicted_poisson_lognormal_hierarchical_nclp', iter*chains*input_data$n_obs), 
                    rep('predicted_poisson_lognormal_hierarchical_cp', iter*chains*input_data$n_obs),
                    rep('observed', input_data$n_obs)), 
                  levels = c('observed',
                             'predicted_poisson_lognormal_hierarchical_nclp',
                             'predicted_poisson_lognormal_hierarchical_cp')),
  value= c(
    as.vector(samples_poisson_lognormal_hierarchical_nclp$pop_post_pred),
    as.vector(samples_poisson_lognormal_hierarchical_cp$pop_post_pred),
    input_data$pop)
)

ggplotly(ggplot(pop_posterior_lognormal_hierarchical, aes(x=value, fill=source, after_stat(density)))+
           geom_histogram(bins=50, position = 'identity', alpha=0.7)+
           theme_minimal())


# New parametrization: non-centering the scale ---------------------------------


pars_lognormal_hierarchical_nclsp <- c('alpha_national','delta_settlement', 
                                       'sigma', 'pop_post_pred')

fit_poisson_lognormal_hierarchical_nclsp <- stan(
  file= here('tutorials', 'refresher','poisson_hierarchical_nclsp.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal_hierarchical_nclsp
)

samples_poisson_lognormal_hierarchical_nclsp<- rstan::extract(fit_poisson_lognormal_hierarchical_nclsp)

# posterior predictive check
pop_posterior_lognormal_hierarchical_param <- tibble(
  source = factor(c(rep('predicted_poisson_lognormal_hierarchical_nclsp', iter*chains*input_data$n_obs),
                    rep('predicted_poisson_lognormal_hierarchical_nclp', iter*chains*input_data$n_obs), 
                    rep('predicted_poisson_lognormal_hierarchical_cp', iter*chains*input_data$n_obs),
                    rep('observed', input_data$n_obs)), 
                  levels = c('observed',
                             'predicted_poisson_lognormal_hierarchical_nclsp',
                             'predicted_poisson_lognormal_hierarchical_nclp',
                             'predicted_poisson_lognormal_hierarchical_cp')),
  value= c(
    as.vector(samples_poisson_lognormal_hierarchical_nclsp$pop_post_pred),
    as.vector(samples_poisson_lognormal_hierarchical_nclp$pop_post_pred),
    as.vector(samples_poisson_lognormal_hierarchical_cp$pop_post_pred),
    input_data$pop)
)

ggplotly(ggplot(pop_posterior_lognormal_hierarchical, aes(x=value, fill=source, after_stat(density)))+
           geom_histogram(bins=50, position = 'identity', alpha=0.7)+
           theme_minimal())



# Bonus: independent variance --------------------------------------------

# NB: not hierarchical

pars_lognormal_hierarchical_var <- c('alpha_national','alpha_settlement', 
                                     'sigma_settlement', 'pop_post_pred')

fit_poisson_lognormal_hierarchical_var<- stan(
  file= here('tutorials', 'refresher','poisson_hierarchical_variance.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars_lognormal_hierarchical_var
)

traceplot(fit_poisson_lognormal_hierarchical_var, pars = 'sigma_settlement')

samples_poisson_lognormal_hierarchical_var<- rstan::extract(fit_poisson_lognormal_hierarchical_var)

# prior checks
comp_sigma <- as_tibble(samples_poisson_lognormal_hierarchical_var$sigma_settlement)
colnames(comp_sigma) <- c('sigma_settlement_1', 'sigma_settlement_2')
comp_sigma <- comp_sigma %>% 
  pivot_longer(everything(),names_to = 'posterior') %>% 
  mutate(model='independant sigma') 

comp_sigma <- rbind(
  comp_sigma,
  tibble(
    posterior = 'sigma_national',
    value = samples_poisson_lognormal_hierarchical_cp$sigma,
    model = 'fixed sigma'
  )
)

ggplot(comp_sigma, aes(x=posterior, y=value, fill=model))+
  geom_boxplot()+
  theme_minimal()

# posterior predictive check
pop_posterior_lognormal_hierarchical_var<- tibble(
  source = factor(c(rep('predicted_poisson_lognormal_hierarchical_var', iter*chains*input_data$n_obs),
                    rep('predicted_poisson_lognormal_hierarchical_cp', iter*chains*input_data$n_obs),
                    rep('observed', input_data$n_obs)), 
                  levels = c('observed',
                             'predicted_poisson_lognormal_hierarchical_var',
                             'predicted_poisson_lognormal_hierarchical_cp')),
  value= c(
    as.vector(samples_poisson_lognormal_hierarchical_var$pop_post_pred),
    as.vector(samples_poisson_lognormal_hierarchical_cp$pop_post_pred),
    input_data$pop)
)

ggplotly(ggplot(pop_posterior_lognormal_hierarchical_var, aes(x=value, fill=source, after_stat(density)))+
           geom_histogram(bins=30, position = 'identity', alpha=0.7)+
           theme_minimal())
