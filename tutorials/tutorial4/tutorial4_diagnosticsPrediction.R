###LOAD WP LOGO##
htmltools::img(src = knitr::image_uri("./assets/pic/320px-UNFPA_logo.svg.png"),
               alt = 'logo', style = 'position:absolute; top:60px; right:0; padding:20px; width: 25%; height: auto')


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

## 
## install.packages('raster')
## install.packages('RColorBrewer')
## install.packages('sf')
## install.packages('tmap')

# 1. Model diagnostics ----

# load data
data <- readxl::read_excel(here('tutorials/data/nga_demo_data.xls'))
data <- data %>% 
  mutate(
    id = as.character(1:nrow(data)),
    pop_density=N/A
  )
# mcmc settings
chains <- 4
iter <- 500
seed <- 1789

# stan data
stan_data <- list(
  population = data$N,
  n = nrow(data),
  area = data$A)

# set parameters to monitor
pars <- c('mu', 'sigma', 'density_hat', 'population_hat')

# 1.1 Short warmup period ----

# set short warmup
warmup <- 20

# mcmc
fit_lowWarmup <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model2.stan'), 
                          data = stan_data,
                          iter = warmup + iter, 
                          chains = chains,
                          warmup = warmup, 
                          pars = pars,
                          seed = seed)

traceplot(fit_lowWarmup, c('mu', 'sigma'), inc_warmup=T)

# 1.2 Wrong prior ----

warmup <- 250

# mcmc
fit_wrong <- rstan::stan(file = file.path('tutorials/tutorial4/tutorial1_model2_wrong.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

traceplot(fit_wrong, c('mu', 'sigma'))

# 2.3 Predicted posterior check ----

# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model2.stan'), 
                          data = stan_data,
                          iter = warmup + iter, 
                          chains = chains,
                          warmup = warmup, 
                          pars = pars,
                          seed = seed)

# extract estimated mu
mus <- data.frame(
    # extract parameter
    posterior=extract(fit, pars='mu')$mu,
    parameter = 'mu'
    )
glimpse(mus)

# retrieve stan parameter
post_warmup_draws <- iter - warmup

# simulate from the prior distribution
mus$prior <- rnorm(chains * post_warmup_draws, 5, 4)

# build dataframe with parameters distribution comparison
parameters <-  rbind(
  # alpha
  mus ,
  #sigma
  data.frame(
      posterior=extract(fit, pars='sigma')$sigma,
      prior = runif(chains * post_warmup_draws, 0, 4),
      parameter = 'sigma')
  )  %>% 
  pivot_longer(-parameter,
    names_to = 'distribution',
    values_to = 'value'
  )

# plot distribution comparison for both parameter
ggplotly(ggplot(parameters, aes(x=value, color=distribution, fill=distribution))+
  geom_density()+
  theme_minimal()+
    facet_wrap(.~parameter, scales = 'free'))

# 2.4 Cross validation of predictions ----

set.seed(2004)
# sample observations
train_size <-  round(nrow(data)*0.7)
train_idx <- sample(1:nrow(data), size = train_size, replace=F)

# build train datasets
train_data <- data[train_idx,]

# build test datasets
test_data <- data[-train_idx,]



# prepare data
stan_data_xval <- list(
  population_train = train_data$N,
  n_train = nrow(train_data),
  n_test = nrow(test_data),

  area_train = train_data$A,
  area_test = test_data$A,

  seed=seed
)

# mcmc setting
pars <- c('mu','sigma', 'population_hat',  'density_hat')

# mcmc
fit_xval <- rstan::stan(file = file.path('tutorials/tutorial4/tutorial1_model2xval.stan'), 
                   data = stan_data_xval,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# predict population
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
    # merge with observations
    left_join(reference_data %>% 
                rename('reference'=all_of(obs)) %>% 
                select(id, reference), by = 'id') %>%
    # 
    mutate(
      residual= predicted_mean - reference,
      ci_size = predicted_upper- predicted_lower,
      estimate = estimate
      )
return(predicted_pop)
}

# build comparison dataframe between in-sample and xvalidation
comparison_df <- rbind(
 getPopPredictions(fit) %>% 
   mutate(Model='In-sample'),
 getPopPredictions(fit_xval, reference_data = test_data) %>% 
   mutate(Model='Cross-validation'))

# compute goodness-of-fit metrics
comparison_df %>% group_by(Model) %>% 
  summarise( `Bias`= mean(residual),
    `Inaccuracy` = mean(abs(residual)),
        `Imprecision` = sd(residual)
) %>%  kbl(caption = 'Goodness-of-metrics computed in-sample vs cross-validation') %>% kable_minimal()
