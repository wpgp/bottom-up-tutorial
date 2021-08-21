###LOAD WP LOGO##
htmltools::img(src = knitr::image_uri("../../assets/pic/320px-UNFPA_logo.svg.png"),
               alt = 'logo', style = 'position:absolute; top:60px; right:0; padding:20px; width: 25%; height: auto')

# 1 Set-up ---

# load libraries
library(tidyverse) # managing data
library(ggdag) # drawing DAG
library(kableExtra) # visualising table
library(here) # handling path
library(rstan) # running Bayesian models

# draw linear model DAG
dagify(
    Y ~ mu,
    Y ~ sigma,
    mu ~ alpha,
    mu ~ beta,
    mu ~ X,
    outcome = 'Y'
  ) %>%
    tidy_dagitty(seed=11) %>% 
    mutate(color=c('data','parameter',  'parameter','parameter',  'parameter','data')) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_text(col = "grey20",size=6,  parse=T) +
    scale_shape_manual(values=c(15,19))+
    theme_dag()+ labs(title = 'Graph of a linear regression', color='', shape='')

# stan setup
options(mc.cores = parallel::detectCores()-1)
rstan::rstan_options(auto_write = TRUE) # speed up running time of compiled model

## install.packages(c( "tidyverse", # for data manipulation
##                     'kableExtra', # for good visualisation of tables
##                     'here' # for handling relative path to external assets (pic, data)
##                     ),
##                  dependencies = TRUE)

# 2 Introduce the data ---

## 
## # download  tutorial data
## download.file(
##   "https://www.pnas.org/highwire/filestream/949050/field_highwire_adjunct_files/1/pnas.1913050117.sd01.xls",
##   'tutorials/data/nga_demo_data.xls',
##   method='libcurl',
##   mode='wb'
## )

# map sample locations
knitr::include_graphics("../../assets/pic/tuto1_nga_mez.PNG")

#load data
data <- readxl::read_excel(here('tutorials/data/nga_demo_data.xls'))
# create unique cluster id
data <- data %>% 
  mutate(
    id= paste0('cluster_',(1:n())) # compute cluster id
  )
data %>% select(id,N, A) %>% head(10) %>% kbl() %>% kable_minimal()

# plot population count
ggplot(data, aes(x=N, y=..density..))+
  geom_histogram(bins=50)+
  geom_density(aes(x=N,y=..density..), size=1, color='orange')+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "", y='', x='')+
  annotate("text",x=1500, y=0.0015, 
           label=paste0('mean=',round(mean(data$N)),' people',
                        '\nstandard deviation=', round(sd(data$N)), ' people'))

# 3 Model1: Normal distribution ---

# Normal model DAG
dagify(
  Population ~ alpha,
  Population ~ sigma,
  outcome = 'Population'
  ) %>%
  tidy_dagitty(seed=41) %>% 
  mutate(color=c('parameter',  'parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=6,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model 1: Normal distribution of population count', color='', shape='')

# plot alpha prior 
ggplot(data %>% mutate(color='Observed data'), aes(x=N, y=..density..))+
  geom_histogram(bins=50, aes(fill=color, color=color))+
  geom_density(data=data.frame(x=rnorm(1000, 0,5000), color=rep( 'Prior distribution for alpha', 1000)), aes(x=x,y=..density.., color=color), size=1)+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "", y='', x='', color='')+ 
  guides(fill=FALSE)

## // Model 1: Population count as a normal distribution

## 
## data{

##   int<lower=0> n; // number of microcensus clusters

##   real<lower=0> population[n]; // count of people

## }

## 
## parameters{

##   // intercept

##   real alpha;

##   // variance

##   real<lower=0> sigma;

## }

## 
## model{

##   // population totals

##   population ~ normal( alpha, sigma );

##   // intercept

##   alpha ~ normal(0, 5000);

##   // standard deviation

##   sigma ~ uniform(0, 1000);

## }

## 

# prepare data for stan
stan_data <- list(
  population = data$N,
  n = nrow(data))

# mcmc settings
chains <- 4
warmup <- 250
iter <- 500
seed <- 1789


# parameters to monitor
pars <- c('alpha','sigma')

# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model1.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
stan_trace(fit, inc_warmup = T)

# plot estimated parameters
mean_pop <- mean(data$N)
sd_pop <- sd(data$N)

alpha_plot <- stan_plot(fit, 'alpha',fill_color='orange')+
  scale_x_continuous(limits=c(400,500))+
  annotate('segment',x=mean_pop, xend=mean_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=mean_pop, 
           y=1.5, col='grey40',label= paste0('Observed average\n',round(mean_pop,2)), fontface =2, size=4.5)
sigma_plot <- stan_plot(fit, 'sigma', fill_color='orange')+
  annotate('segment',x=sd_pop, xend=sd_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=sd_pop, 
           y=1.5, col='grey40',
           label= paste0('Observed standard deviation \n',round(sd_pop,2)), fontface =2, size=4.5)

gridExtra::grid.arrange(alpha_plot, sigma_plot, nrow=2)


## // Model 1bis: Population count as a normal distribution with integrated predictions

## 
## data{

##   int<lower=0> n; // number of microcensus clusters

##   real<lower=0> population[n]; // count of people

## }

## 
## parameters{

##   // intercept

##   real alpha;

##   // variance

##   real<lower=0> sigma;

## }

## 
## model{

##   // population totals

##   population ~ normal( alpha, sigma );

##   // intercept

##   alpha ~ normal(0, 5000);

##   // standard deviation

##   sigma ~ uniform(0, 1000);

## }

## 
## generated quantities{

##    real population_hat[n];

## 
##    for(idx in 1:n){

##      population_hat[idx] = normal_rng( alpha, sigma );

##    }

## 

## }

## 

# parameter to monitor
pars <- c('alpha','sigma', 'population_hat')

# mcmc
fit_model1 <- rstan::stan(file = file.path('./tutorial1_model1bis.stan'), 
                          data = stan_data,
                          iter = warmup + iter, 
                          chains = chains,
                          warmup = warmup, 
                          pars = pars,
                          seed = seed)


# extract predictions
predicted_pop_model1 <- as_tibble(extract(fit_model1, 'population_hat')$population_hat)

colnames(predicted_pop_model1) <- data$id

predicted_pop_model1 %>% 
  mutate(iteration= paste0('iter_', 1:(iter*chains))) %>% 
  select(iteration, 1:10) %>% head(10) %>% kbl() %>% kable_minimal()

# plot posterior prediction for one cluster
ggplot(predicted_pop_model1, aes(x=cluster_1))+
  geom_density(size=1.5, color='orange')+
  theme_minimal()+
    theme(axis.text.y = element_blank())+
  labs(title = "Population prediction for cluster 1 ", y='', x='')

# summarize predictions
comparison_df <- predicted_pop_model1 %>% 
      pivot_longer(everything(),names_to = 'id', values_to = 'predicted') %>% 
      group_by(id) %>% 
      summarise(across(everything(), list(mean=~mean(.), 
                                          upper=~quantile(., probs=0.975), 
                                          lower=~quantile(., probs=0.025))))
comparison_df %>% head() %>% kbl() %>% kable_minimal()

# add observed values
comparison_df <- comparison_df %>% 
  left_join(data %>% 
              select(id, N), by = 'id')

# plot predicted vs observed
ggplot(comparison_df) +
  geom_pointrange(aes(x=N, y=predicted_mean, ymin=predicted_lower, ymax=predicted_upper
                      ),
                   fill='grey50', color='grey70', shape=21
                  )+
  geom_abline(slope=1, intercept = 0, color='orange', size=1)+
  theme_minimal()+
  labs(title = '', x='Observed population count', y='Predicted population')

# compute goodness-of-fit metrics
comparison_df %>%
  mutate(residual = predicted_mean-N,
          in_CI = ifelse(N>predicted_lower &N<predicted_upper, T, F)) %>% 
  summarise(
    `Bias`= mean(residual),
    `Imprecision` = sd(residual),
    `Inaccuracy` = mean(abs(residual)),
    `Correct credible interval (in %)` = round(sum(in_CI)/n()*100,1),
    R2 = cor(predicted_mean, N)^2
  ) %>% 
    kbl(caption = "Normal model goodness-of-fit metrics") %>% kable_minimal()

# 4 Model2: Poisson-Lognormal distribution ---

# compute population density
data <- data %>% 
  mutate(
    pop_density = N/A 
  )
# plot population density
ggplot(data, aes(x=pop_density, y=..density..))+
  geom_histogram(bins=50)+
  geom_density(size=1, color='orange')+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "", y='', x='')+
  annotate("text",x=500, y=0.0035, 
           label=paste0('mean=',round(mean(data$pop_density)),' people/hectare',
                        '\nvariance=', round(var(data$pop_density)), ' people/hectare'))

# compare population density and population count
ggplot(data %>% 
         select(id, N, pop_density) %>% 
         rename("Population count"=N,
                "Population density"=pop_density) %>% 
         pivot_longer(-id) %>% 
         mutate(idx=0), aes(x=value, y=idx))+
  geom_jitter()+
  geom_density(aes(x=value,y=..density..*200), size=1, color='orange')+
  scale_y_continuous(limits=c(-1,1))+
  facet_wrap(vars(name),  dir='v')+
  theme_minimal()+
  labs(y='', x='')+
  theme(axis.text.y = element_blank(), strip.text.x = element_text(size = 20))

## // Model 2: Population count as a Poisson-Lognormal distribution

## 
## data{

##   int<lower=0> n; // number of microcensus clusters

##   int<lower=0> population[n]; // count of people

##   vector<lower=0>[n] area; // settled area

## }

## 
## parameters{

##   // population density

##   vector<lower=0>[n] pop_density;

##   // intercept

##   real alpha;

##   // variance

##   real<lower=0> sigma;

## }

## 
## model{

##   // population totals

##   population ~ poisson(pop_density .* area);

##   pop_density ~ lognormal( alpha, sigma );

##   // intercept

##   alpha ~ normal(0, 15);

##   // variance

##   sigma ~ uniform(0, 5);

## }

## 
## generated quantities{

##    int<lower=0> population_hat[n];

##    real<lower=0> density_hat[n];

## 
##    for(idx in 1:n){

##      density_hat[idx] = lognormal_rng( alpha, sigma );

##      population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);

##    }

## 

## }

## 

# prepare data for stan
stan_data_model2 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A)


# set paameters to monitor
pars <- c('alpha','sigma', 'population_hat', 'density_hat')

# mcmc
fit_model2 <- rstan::stan(file = file.path('tutorial1_model2.stan'), 
                   data = stan_data_model2,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
traceplot(fit_model2, c('alpha', 'sigma'))

# extract posterior predictions
predicted_pop_model2 <- as_tibble(extract(fit_model2, 'population_hat')$population_hat)
predicted_dens_model2 <- as_tibble(extract(fit_model2, 'density_hat')$density_hat)
colnames(predicted_pop_model2) <- data$id
colnames(predicted_dens_model2) <- data$id

# summarise posterior predictions
comparison_df <- rbind(predicted_dens_model2 %>% 
   pivot_longer(everything(),names_to = 'id', values_to = 'predicted') %>% 
   group_by(id) %>% 
   summarise(across(everything(), list(mean=~mean(.), 
                                       upper=~quantile(., probs=0.975), 
                                       lower=~quantile(., probs=0.025)))) %>% 
   mutate(source= 'Poisson-Lognormal model',
          type= 'Population density') %>% 
   left_join(data %>% 
               select(id, pop_density)%>% 
              rename(reference=pop_density), by = 'id'),
  predicted_pop_model2 %>% 
  pivot_longer(everything(),names_to = 'id', values_to = 'predicted') %>% 
  group_by(id) %>% 
  summarise(across(everything(), list(mean=~mean(.), 
                                      upper=~quantile(., probs=0.975), 
                                      lower=~quantile(., probs=0.025)))) %>% 
  mutate(source= 'Poisson-Lognormal model',
         type='Population count') %>% 
  left_join(data %>% 
              select(id, N) %>% 
              rename(reference=N), by = 'id'))
# plot posterior predictions
ggplot(comparison_df %>% 
         mutate(type= factor(type, levels=c('Population density', 'Population count')))) +
  geom_pointrange(aes(x=reference, y=predicted_mean, ymin=predicted_lower, ymax=predicted_upper
                      ),
                   fill='grey50', color='grey70', shape=21
                  )+
  geom_abline(slope=1, intercept = 0, color='orange', size=1)+
  theme_minimal()+
  labs(title = '', x='Observations', y='Predictions')+ 
  facet_wrap(.~type, scales = 'free')


# compute goodness-of-fit metrics
comparison_df %>%
  filter(type=='Population count') %>% 
  mutate(residual = predicted_mean-reference,
          in_CI = ifelse(reference>predicted_lower &reference<predicted_upper, T, F)) %>% 
  summarise(
    `Bias`= mean(residual),
    `Imprecision` = sd(residual),
    `Inaccuracy` = mean(abs(residual)),
    `Correct credible interval (in %)` = round(sum(in_CI)/n()*100,1),
    R2 = cor(predicted_mean, reference)^2
  ) %>% 
    kbl(caption = "Poisson-Lognormal model goodness-of-fit metrics") %>% kable_minimal()

# 5 Test prior ---
# set parameters to monitor
pars <- c('alpha','sigma')

# mcmc
fit_wrong <- rstan::stan(file = file.path('tutorial1_model1wrong.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
traceplot(fit_wrong)
