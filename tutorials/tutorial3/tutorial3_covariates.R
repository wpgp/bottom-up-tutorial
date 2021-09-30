# 1 Set-up ---

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

# raster example
knitr::include_graphics(here("./assets/pic/tuto3_rasterZoom.png"))

dagify(
    Y ~ alpha_1+alpha_2+ alpha_3+sigma + beta+ X,
    alpha_1 ~ alpha,
    alpha_2 ~ alpha,
    alpha_3 ~ alpha,
    outcome = 'Y',
    latent = 'alpha'
  ) %>%
    tidy_dagitty(seed=7) %>% 
    mutate(color=c('data','parameter','parameter','parameter','parameter','parameter','parameter','parameter','parameter','data')) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_text(col = "grey20",size=4,  parse=T) +
    scale_shape_manual(values=c(15,19))+
    theme_dag()+ labs(title = '', color='', shape='')

review_cov <- read_csv('tutorials/tutorial3/covs_review.csv')

review_cov %>% arrange(Type) %>% kbl(caption='Review of covariates used in WorldPop bottom-up population models') %>% kable_minimal()

# prepare data
data <- readxl::read_excel(here('tutorials/data/nga_demo_data.xls'))
data <- data %>% 
  mutate(
    pop_density=N/A,
    id = as.character(1:nrow(data))
  )

data_long <- data %>% 
  pivot_longer(starts_with('x'), names_to = 'cov')

ggplot(data_long, aes(x=pop_density,y=value))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE,color='orange')+
  theme_minimal()+
  facet_wrap(.~cov, ncol=3, scales = 'free')+
  labs(x='Population density', y='')


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

covs_scaled <-  covs %>% 
  mutate(cluster_id = 1:nrow(covs)) %>% 
  pivot_longer(-cluster_id,names_to = 'cov') %>% 
  left_join(scale_factor, by="cov") %>% 
  mutate(value= (value-cov_mean)/cov_sd ) %>% 
  select(-cov_mean, -cov_sd) %>% 
  pivot_wider(names_from = cov, values_from = value, id_cols = cluster_id) %>% 
  select(-cluster_id)


write_csv(covs_scaled, 'tutorials/data/covs_scaled.csv')
write_csv(scale_factor, 'tutorials/data/scale_factor.csv')

## // Model 1: Independent alpha by settlement type

## 
## data{

##   ...

##   // slope

##   int<lower=0> ncov; // number of covariates

##   matrix[n, ncov] cov; // covariates

## }

## parameters{

##   ...

##   // slope

##   row_vector[ncov] beta;

## }

## transformed parameters{

##   ...

##   for(idx in 1:n){

##     pop_density_mean[idx] = alpha_t_r[type[idx], region[idx]] + sum( cov[idx,] .* beta );

##   }

## }

## model{

##   ...

##   //slope

##   beta ~ normal(0,10);

## }

## generated quantities{

##   ...

##    for(idx in 1:n){

##     density_hat[idx] = lognormal_rng( alpha_t_r[type[idx], region[idx]] + sum(cov[idx,] .* beta), sigma );

##    }

## }


# mcmc settings
chains <- 4
warmup <- 500
iter <- 500
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


inits.out <- list()
set.seed(stan_data_model1$seed)

for (c in 1:chains){
  inits.i <- list()
  # intercept
  inits.i$pop_density <- rlnorm(stan_data_model1$n, log(stan_data_model1$population / stan_data_model1$area), 0.5)
  inits.i$sigma <- runif(1, 0.4, 0.8)
  inits.i$alpha <- runif(1, 3, 6)
  inits.i$nu_alpha <- runif(1, 0.5, 1.5)
  inits.i$nu_alpha_t <- runif(1, 0.5, 1.5)
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

stan_plot(fit1, pars='beta', fill_color='orange')

fit0 <- readRDS('tutorials/tutorial2/tutorial2_model3_fit.rds')

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


## // Model 1: Independent alpha by settlement type

## 
## data{

##   ...

##     // fixed slope

##   int<lower=0> ncov_fixed; // number of covariates -1

##   matrix[n, ncov_fixed] cov_fixed; // covariates

##   // random slope

##   vector[n] cov_random;

## }

## parameters{

##   ...

##   // slope

##   row_vector[ncov_fixed] beta_fixed;

##   vector[ntype] beta_random;

## }

## transformed parameters{

##   ...

##   vector[n] beta;

## 
##   for(idx in 1:n){

##     beta[idx] = sum( cov_fixed[idx,] .* beta_fixed) + cov_random[idx] * beta_random[type[idx]];

##     pop_density_mean[idx] = alpha_t_r[type[idx], region[idx]] + beta[idx];

##   }

## }

## model{

##   ...

##   //slope

##   beta_fixed ~ normal(0,10);

##   beta_random ~ normal(0,10);

## }

## generated quantities{

##   ...

##  vector[n] beta_hat;

## 
##   for(idx in 1:n){

##     beta_hat[idx] = sum( cov_fixed[idx,] .* beta_fixed) + cov_random[idx] * beta_random[type[idx]];

##     density_hat[idx] = lognormal_rng( alpha_t_r[type[idx], region[idx]] + beta_hat[idx], sigma );

##   ...

## }


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

inits.out <- list()
set.seed(stan_data_model2$seed)
for (c in 1:chains){
  inits.i <- list()
  # intercept
  inits.i$pop_density <- rlnorm(stan_data_model2$n, log(stan_data_model2$population / stan_data_model2$area), 0.5)
  inits.i$sigma <- runif(1, 0.4, 0.8)
  inits.i$alpha <- runif(1, 3, 6)
  inits.i$nu_alpha <- runif(1, 0.5, 1.5)
  inits.i$nu_alpha_t <- runif(1, 0.5, 1.5)
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

stan_plot(fit2, pars='beta_random', fill_color='orange')

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

saveRDS(fit2, 'tutorials/tutorial3/tutorial3_model2_fit.rds')
