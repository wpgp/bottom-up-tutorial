# 1 Set-up ----

# load libraries
library(tidyverse) # managing data
library(ggdag) # drawing DAG
library(kableExtra) # visualising table
library(here) # handling path
library(rstan) # running Bayesian models
library(plotly) # interactive plot

# stan setup
options(mc.cores = parallel::detectCores()-1)
rstan::rstan_options(auto_write = TRUE) # speed up running time of compiled model







# 2. Covariates preparation ----
# load data
data <- readxl::read_excel(here('tutorials/data/nga_demo_data.xls'))
data <- data %>% 
  mutate(
    pop_density=N/A,
    id = as.character(1:nrow(data))
  )

# contrast covariates with pop density
data_long <- data %>% 
  pivot_longer(starts_with('x'), names_to = 'cov')

ggplot(data_long, aes(x=pop_density,y=value))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE,color='orange')+
  theme_minimal()+
  facet_wrap(.~cov, ncol=3, scales = 'free')+
  labs(x='Population density', y='')


# compute scaling factors (mean and sd)
covariatesScaling <- function(var){
  mean_var <- mean(var)
  sd_var <- sd(var)
  return(
    data.frame(
      'cov_mean'= mean_var,
      'cov_sd' = sd_var
    )
  )
} 

covs <- data %>% 
  select(starts_with('x'))

scale_factor <- bind_rows(apply(covs, 2, covariatesScaling))
scale_factor$cov <- colnames(covs)

scale_factor %>% select(cov, cov_mean, cov_sd) %>% kbl %>%  kable_minimal()

# apply scaling factors to covariates
covs_scaled <-  covs %>% 
  mutate(cluster_id = 1:nrow(covs)) %>% 
  pivot_longer(-cluster_id,names_to = 'cov') %>% 
  left_join(scale_factor, by="cov") %>% 
  mutate(value= (value-cov_mean)/cov_sd ) %>% 
  select(-cov_mean, -cov_sd) %>% 
  pivot_wider(names_from = cov, values_from = value, id_cols = cluster_id) %>% 
  select(-cluster_id)


# save scaling factor
write_csv(covs_scaled, here('tutorials/data/covs_scaled.csv'))
write_csv(scale_factor, here('tutorials/data/scale_factor.csv'))



# 3. Modelling with covariates ----

# mcmc settings
chains <- 4
warmup <- 700
iter <- 700
seed <- 1789

# prepare data for stan
stan_data_model1 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A,
  type = data$type,
  ntype= n_distinct(data$type),
  region = data$region,
  nregion = n_distinct(data$region),
  seed=seed,
  cov = covs_scaled,
  ncov = ncol(covs_scaled)
  )

pars <- c('alpha','sigma','beta','alpha_t', 'nu_alpha', 'nu_alpha_t', 'population_hat',  'density_hat')

# mcmc
fit1 <- rstan::stan(file = file.path('tutorials/tutorial3/tutorial3_model1.stan'), 
                   data = stan_data_model1,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# add initialisation
inits.out <- list()
set.seed(stan_data_model1$seed)

for (c in 1:chains){
  inits.i <- list()

  inits.i$sigma <- runif(1, 0.4, 0.8)
  inits.i$alpha <- runif(1, 3, 6)
  inits.i$beta <- runif(stan_data_model1$ncov, -1, 1)
  
  inits.out[[c]] <- inits.i
}

fit1bis <- rstan::stan(file = file.path('tutorials/tutorial3/tutorial3_model1.stan'), 
                   data = stan_data_model1,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed,
                   init= inits.out)

# plot beta estimation
stan_plot(fit1, pars='beta', fill_color='orange')

# load tutorial 2 final model
fit0 <- readRDS('tutorials/tutorial2/tutorial2_model3_fit.rds')

# extract predictions
getPopPredictions <- function(model_fit, 
                              estimate='population_hat',
                              obs='N', reference_data=data){
  # extract predictions
  predicted_pop <- as_tibble(extract(model_fit, estimate)[[estimate]])
  colnames(predicted_pop) <- reference_data$id
  
  # summarise predictions
  predicted_pop <- predicted_pop %>% 
    pivot_longer(everything(),names_to = 'id', values_to = 'predicted') %>% 
    group_by(id) %>% 
    summarise(
      across(everything(), list(mean=~mean(.), 
                                upper=~quantile(., probs=0.975), 
                                lower=~quantile(., probs=0.025)))
      ) %>% 
    left_join(reference_data %>% 
                rename('reference'=all_of(obs)) %>% 
                select(id, reference), by = 'id')%>% 
    mutate(
      residual= predicted_mean - reference,
      ci_size = predicted_upper- predicted_lower,
      estimate = estimate
      )

return(predicted_pop)
}

comparison_df <- rbind(
 getPopPredictions(fit0) %>% 
   mutate(Model='Without covariates'),
 getPopPredictions(fit1) %>% 
   mutate(Model='With covariates'))

# compute goodness-of-fit metrics
comparison_df %>% group_by(Model) %>% 
  summarise( `Bias`= mean(residual),
    `Inaccuracy` = mean(abs(residual)),
        `Imprecision` = sd(residual)
) %>%  kbl(caption = 'Goodness-of-metrics comparison with and without covariates ') %>% kable_minimal()

# 4. Modelling covariates with random slope ----

ggplot(data_long %>% 
         group_by(type) %>% 
         mutate(
           type = paste0(type,' n=' ,n()),
           type=as.factor(type)), aes(x=pop_density,y=value, color=type))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()+
  facet_wrap(.~cov, ncol=3, scales = 'free')+
  labs(y='', x='Population density', color='Settlement type')




# prepare stan data
stan_data_model2 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A,
  type = data$type,
  ntype= n_distinct(data$type),
  region = data$region,
  nregion = n_distinct(data$region),
  seed=seed,
  cov_fixed = covs_scaled %>% select(-x4),
  ncov_fixed = ncol(covs_scaled) -1,
  cov_random = covs_scaled$x4
  )

pars <- c('alpha','sigma','beta_fixed','beta_random','alpha_t','alpha_t_r', 'nu_alpha', 'nu_alpha_t', 'population_hat',  'density_hat')

# initialise
inits.out <- list()
set.seed(stan_data_model2$seed)

for (c in 1:chains){
  inits.i <- list()
  # intercept
  inits.i$sigma <- runif(1, 0.4, 0.8)
  inits.i$alpha <- runif(1, 3, 6)
  inits.i$beta_fixed <- runif(stan_data_model2$ncov_fixed, -1, 1)
  inits.i$beta_random <- runif(stan_data_model2$ntype, -1, 1)

  inits.out[[c]] <- inits.i
}

# mcmc
fit2 <- rstan::stan(file = file.path('tutorials/tutorial3/tutorial3_model2.stan'), 
                   data = stan_data_model2,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed,
                   init = inits.out)

# plot beta estimation
stan_plot(fit2, pars='beta_random', fill_color='orange')+
    # add alpha from tutorial 1
  geom_vline(xintercept=-0.006515444, size=1.5, linetype=2)+
  annotate('text', x=0.1, y=5.7, label="beta for cov 4 \nfrom first model")

# extract predictions
comparison_df <- rbind(
 getPopPredictions(fit1) %>% 
   mutate(model='Fixed effect'),
 getPopPredictions(fit2) %>% 
   mutate(model='Random effect in x4'))
# compute goodness-of-fit metrics
comparison_df %>% group_by(model) %>% 
  summarise( `Bias`= mean(residual),
    `Inaccuracy` = mean(abs(residual)),
        `Imprecision` = sd(residual)
) %>%  kbl(caption = 'Goodness-of-metrics comparison with and without random effect in x4 ') %>% kable_minimal()

# save model
saveRDS(fit2, 'tutorials/tutorial3/tutorial3_model2_fit.rds')
