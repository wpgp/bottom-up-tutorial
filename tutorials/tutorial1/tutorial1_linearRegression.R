# 1 Set-up ----

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
options(mc.cores = parallel::detectCores()) #set up the maximum number of cores used by stan
rstan::rstan_options(auto_write = TRUE) # speed up running time 

## install.packages(c( "tidyverse", # for data manipulation
##                     'kableExtra', # for good visualisation of tables
##                     'here' # for handling relative path to external assets (pic, data)
##                     ),
##                  dependencies = TRUE)

# 2 Simulated data ----

# Simulate data
seed <- 2004
set.seed(seed)
data <- tibble(y=rnorm(1e3, mean= 5, sd=50))

# plot simulated data
ggplot(data, aes(x=y))+
  geom_histogram(bins = 50)+
  theme_minimal()+
  geom_vline(xintercept = mean(data$y), color='orange', size=1)+
  annotate('text', x=15, y=30, label=paste0('Observed mean: ', round(mean(data$y),2)), colour='orange', angle=90)+
  geom_segment(x=0,y=1, xend=sd(data$y), yend=1, colour='orange', size=1)+
  annotate('text', x=50, y=4, label=paste0('Observed sd: ', round(sd(data$y),2)), colour='orange')+
  labs(y='', x='observed y')+
  theme(axis.text.y = element_blank())

# Normal model DAG
dagify(
  Y ~ mu,
  Y ~ sigma) %>%
  tidy_dagitty(seed=41) %>% 
  mutate(color=c('parameter', 'parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=6,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model with simulated data: Normal distribution of Y', color='', shape='')


# define mu prior
data$prior_mu <- rnorm(1e3, mean= 0, sd=100)

ggplot(data)+
  geom_histogram(aes(x=y, after_stat(density)), bins=50, fill='grey30')+
  geom_density(aes(x=prior_mu), colour='#00BFC4', size=1)+
  theme_minimal()+
  annotate('text', y=0.0020, x=250, label=paste0('Normal(0,100)'), colour='#00BFC4', size=5)+
    theme(axis.text.y = element_blank())+
  labs(y='', x='observed y')

# define sigma prior
data$prior_sigma <- runif(1e3,min = 0, max = 200)

ggplot(data)+
  geom_histogram(aes(x=y, after_stat(density)), bins=50, fill='grey30')+
  geom_density(aes(x=prior_sigma), colour='#00BFC4', size=1, trim=T)+
  theme_minimal()+
  annotate('text', y=0.0060, x=150, label=paste0('Uniform(0,200)'), colour='#00BFC4', size=5)+
      theme(axis.text.y = element_blank())+
  labs(y='', x='observed y')



# prepare data for stan
stan_data <- list(
  y = data$y,
  n = nrow(data))

# mcmc settings
chains <- 4
warmup <- 250
iter <- 500

# parameters to monitor
pars <- c('mu','sigma')

# mcmc
fit <- rstan::stan(file = file.path('tutorial1_model.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
stan_trace(fit, inc_warmup = T)

# summarise estimated parameters
estimated <- summary(fit, pars=pars)$summary

estimated %>% kbl() %>% kable_minimal()

# plot estimated parameters
mean_pop <- mean(data$y)
sd_pop <- sd(data$y)


mu_plot <- stan_plot(fit, 'mu',fill_color='orange')+
  annotate('segment',x=mean_pop, xend=mean_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=mean_pop, 
           y=1.5, col='grey40',label= paste0('Observed\n',round(mean_pop,2)), fontface =2, size=4.5)+
    annotate('segment',x=5, xend=5, 
           y=0.7, yend=1.2,col='grey10', size=1)+
    annotate('text',x=5, 
           y=1.5, col='grey10',label= paste0('True\n',5), fontface =2, size=4.5)+
    annotate('text',x=estimated['mu', 'mean'], 
           y=1.5, col='orange',label= paste0('Estimated\n',round(estimated['mu', 'mean'],2)), fontface =2, size=4.5)
sigma_plot <- stan_plot(fit, 'sigma', fill_color='orange')+
  annotate('segment',x=sd_pop, xend=sd_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=sd_pop, 
           y=1.5, col='grey40',
           label= paste0('Observed\n', round(sd_pop,2)), fontface =2, size=4.5)+
    annotate('segment',x=50, xend=50, 
           y=0.7, yend=1.2,col='grey10', size=1)+
    annotate('text',x=50, 
           y=1.5, col='grey10',label= paste0('True\n',50), fontface =2, size=4.5)+
    annotate('text',x=estimated['sigma', 'mean'], 
           y=1.5, col='orange',label= paste0('Estimated\n',round(estimated['sigma', 'mean'],2)), fontface =2, size=4.5)

gridExtra::grid.arrange(mu_plot, sigma_plot, nrow=2)


# 3 Introduce the data ----


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
    id= paste0('cluster_',(1:n())), # compute cluster id
    pop_density = N/A # compute population density
  )
data %>% select(id,N, A) %>% head(10) %>% kbl() %>% kable_minimal()

# plot population count
ggplot(data, aes(x=N))+
  geom_histogram(bins=50)+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "", y='', x='Observed population count')+
  geom_vline(xintercept = mean(data$N), color='orange', size=1)+
  annotate('text', x=500, y=25, label=paste0('Observed mean: ', round(mean(data$N))), colour='orange', angle=90)


# 4 Model1: Poisson distribution ----

# Poisson model DAG
dagify(
  Population ~ lambda,
  outcome = 'Population'
  ) %>%
  tidy_dagitty(seed=9) %>% 
  mutate(color=c('parameter',  'data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=5,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model 1: Poisson distribution of population count', color='', shape='')

# define lambda prior
data$prior_lambda <- runif(nrow(data), min=0, max=3000)

ggplot(data)+
  geom_histogram(aes(x=N, after_stat(density)), bins=50, fill='grey30')+
  geom_density(aes(x=prior_lambda), colour='#00BFC4', size=1)+
  theme_minimal()+
  annotate('text', y=0.0005, x=2000, label=paste0('Uniform(0,3000)'), colour='#00BFC4', size=5)+
    theme(axis.text.y = element_blank())+
  labs(y='', x='Observed population count')



# prepare data for stan
stan_data <- list(
  population = data$N,
  n = nrow(data))

# parameters to monitor
pars <- c('lambda')

# mcmc
fit <- rstan::stan(file = file.path('tutorial1_model1.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

traceplot(fit, inc_warmup=T)

# plot estimated parameters
mean_pop <- mean(data$N)

stan_plot(fit, 'lambda',fill_color='orange')+
  annotate('segment',x=mean_pop, xend=mean_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=mean_pop, 
           y=1.5, col='grey40',label= paste0('Observed average\n',round(mean_pop,2)), fontface =2, size=4.5)






pars <- c('lambda', 'population_hat')

# mcmc
fit_model1 <- rstan::stan(file = file.path('tutorial1_model1bis.stan'), 
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
    `Correct credible interval (in %)` = round(sum(in_CI)/n()*100,1)
  ) %>% 
    kbl(caption = "Poisson model goodness-of-fit metrics") %>% kable_minimal()

#plot overdispersion
ggplot(data, aes(x=A, y=N))+
  geom_point()+
  theme_minimal()+
  labs(x='Settled area in hectares', y='Population count')

# map sample locations
knitr::include_graphics("../../assets/pic/tuto1_log_normal_distributions.png")

# Lognormal model DAG
dagify(
  Population ~ Pop_density,
  Population ~ Settled_area,
  Pop_density ~ mu,
  Pop_density ~ sigma,
  outcome = 'Population'
  ) %>%
  tidy_dagitty(seed=3) %>% 
  mutate(color=c('parameter','data','parameter','parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=4,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model 2: Lognormal-Poisson distribution of population count', color='', shape='')

# 5 Model2: Lognormal ----

# define prior for mu
data$prior_mu <- rnorm(nrow(data), mean= 5, sd=4)

ggplot(data)+
  geom_histogram(aes(x=log(pop_density), after_stat(density)), bins=100, fill='grey30')+
  geom_density(aes(x=prior_mu), colour='#00BFC4', size=1)+
  theme_minimal()+
  annotate('text', y=0.1, x=15, label=paste0('Normal(5,4)'), colour='#00BFC4', size=5)+
    geom_vline(xintercept = median(log(data$pop_density)), color='orange', size=1)+
  annotate('text', x=5.1, y=0.3, label=paste0('Observed median: ', round(median(log(data$pop_density))), 2), colour='orange', angle=90)+
    theme(axis.text.y = element_blank())+
  labs(y='', x='Observed population density (log)')+
  xlim(-8,17)

# define prior for sigma
data$prior_sigma <- runif(nrow(data), min = 0, max=4)+5

ggplot(data)+
  geom_histogram(aes(x=log(pop_density), after_stat(density)), bins=100, fill='grey30')+
  geom_density(aes(x=prior_sigma), colour='#00BFC4', size=1, trim=T)+
  theme_minimal()+
  annotate('text', y=0.3, x=7.7, label=paste0('Uniform(0,4)'), colour='#00BFC4', size=5)+
    theme(axis.text.y = element_blank())+
  labs(y='', x='Observed population density (log)')



# prepare data for stan
stan_data_model2 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A)

# set parameters to monitor
pars <- c('mu','sigma', 'population_hat', 'density_hat')

# mcmc
fit_model2 <- rstan::stan(file = file.path('tutorial1_model2.stan'), 
                   data = stan_data_model2,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
traceplot(fit_model2, c('mu', 'sigma'))

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
    `Correct credible interval (in %)` = round(sum(in_CI)/n()*100,1)
  ) %>% 
    kbl(caption = "Poisson-Lognormal model goodness-of-fit metrics") %>% kable_minimal()

## saveRDS(fit_model2, 'tutorial1_model2_fit.rds')
