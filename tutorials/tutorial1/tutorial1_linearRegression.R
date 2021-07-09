library(tidyverse)
library(ggplot2)
library(ggdag)
library(rstan)
library(EnvStats)
options(mc.cores = parallel::detectCores()-1)
rstan::rstan_options(auto_write = TRUE)
rstan::rstan_options(javascript = FALSE)
seed <- 1789
# Data --------------------------------------------------------------------


download.file(
  "https://www.pnas.org/highwire/filestream/949050/field_highwire_adjunct_files/1/pnas.1913050117.sd01.xls",
  'tutorials/data/nga_demo_data.xls',
  method='libcurl',
  mode='wb'
)

data <- readxl::read_excel('tutorials/data/nga_demo_data.xls')


# Descriptive stats -------------------------------------------------------



ggplot(data, aes(x=N, y=..density..))+
  geom_histogram(bins=50)+
  geom_density(aes(x=N,y=..density..), size=1, color='orange')+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "Population count distribution", y='', x='')+
  annotate("text",x=1500, y=0.0015, 
           label=paste0('mean=',round(mean(data$N)),' people',
                        '\nvariance=', round(var(data$N)), ' people'))


# Theoretical model -------------------------------------------------------


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
  geom_dag_text(col = "grey20") +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model 1: Normal distribution of population count')





# Model 1: Population count as a normal distribution ----------------------------------------------


# model data
stan_data <- list(
  population = data$N,
  n = nrow(data))


# mcmc settings
chains <- 4
warmup <- 250
iter <- 500


# parameters to monitor
pars <- c('alpha','sigma')


# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   #init = init,
                   pars = pars,
                   control = list(max_treedepth=10, adapt_delta=0.8),
                   seed = seed)

print(fit)
stan_trace(fit)


# Model 2: Population count as a poisson-lognormal compound ----------------
data <- data %>% 
  mutate(
    pop_density = N/A,
    id= paste0('cluster_',(1:n()))
  )

ggplot(data %>% 
         select(id, N, pop_density) %>% 
         rename("Population count"=N,
                "Population density"=pop_density) %>% 
         pivot_longer(-id) %>% 
         mutate(idx=0), aes(x=value, y=idx))+
  geom_jitter()+
  geom_density(aes(x=value,y=..density..*200), size=1, color='orange')+
  scale_y_continuous(limits=c(-1,1))+
  facet_wrap(vars(name), scales = "free_x", dir='v')+
  theme_minimal()+
  labs(y='', x='')+
  theme(axis.text.y = element_blank(), strip.text.x = element_text(size = 20))

ggplot(data, aes(x=pop_density, y=..density..))+
  geom_histogram(bins=50)+
  geom_density(size=1, color='orange')+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "Population density distribution", y='', x='')+
  annotate("text",x=500, y=0.0035, 
           label=paste0('mean=',round(mean(data$pop_density)),' people/hectare',
                        '\nvariance=', round(var(data$pop_density)), ' people/hectare'))

dagify(
  Population ~ pop_density,
  Population ~ Area,
  pop_density ~ sigma,
  pop_density ~ alpha,
  outcome = 'Population',
  labels =c(
    'pop_density' = 'Population \n density',
    'Area' = ' Settled \narea',
    'Population' = 'Population',
    'sigma'='sigma',
    'alpha'='alpha'
  )
) %>%
  tidy_dagitty(seed=8) %>% 
  mutate(type=c('data','parameter',  'parameter','parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=type,shape=type)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20", aes(label=label)) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model 2: Poisson-LogNormal distribution of population count')

# model data
stan_data_model2 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A)

# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model2.stan'), 
                   data = stan_data_model2,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   #init = init,
                   pars = pars,
                   control = list(max_treedepth=10, adapt_delta=0.8),
                   seed = seed)

print(fit)
stan_trace(fit)
stan_trace(fit, inc_warmup = T)
param_plot <- stan_plot(fit)

median_pop_density <- log(median(data$pop_density))
geosd_pop_density <- log(EnvStats::geoSD(data$pop_density))

alpha_plot <- stan_plot(fit, 'alpha',fill_color='orange')+
  scale_x_continuous(limits=c(4.65,4.85))+
  annotate('segment',x=median_pop_density, xend=median_pop_density, 
           y=0.95, yend=1.05,col='grey40', size=1.5)+
  annotate('text',x=median_pop_density, 
           y=1.2, col='grey40',label= paste0('Observed median (log)\n',round(median_pop_density,2)), fontface =2, size=4.5)

sigma_plot <- stan_plot(fit, 'sigma', fill_color='orange')+
  annotate('segment',x=geosd_pop_density, xend=geosd_pop_density, 
           y=0.95, yend=1.05,col='grey40', size=1.5)+
  annotate('text',x=geosd_pop_density, 
           y=1.2, col='grey40',
           label= paste0('Observed geometric \nstandard deviation (log)\n',round(geosd_pop_density,2)), fontface =2, size=4.5)
 
gridExtra::grid.arrange(alpha_plot, sigma_plot, nrow=2)



# Model goodness of fit ---------------------------------------------------
pars <- c('alpha','sigma', 'people_hat')


# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model2bis.stan'), 
                   data = stan_data_model2,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   #init = init,
                   pars = pars,
                   control = list(max_treedepth=10, adapt_delta=0.8),
                   seed = seed)

predicted_pop <- as_tibble(extract(fit, 'people_hat')$people_hat)
colnames(predicted_pop) <- data$id

predicted_pop[,1:10]

comparison_df <- predicted_pop %>% 
  pivot_longer(everything(),names_to = 'id', values_to = 'predicted_N') %>% 
  group_by(id) %>% 
  summarise(across(everything(), list(mean=~mean(.), 
                                      lower=~quantile(., probs=0.975), 
                                      upper=~quantile(., probs=0.025)))) %>% 
  left_join(data %>% 
              select(id, N))

ggplot(comparison_df) +
  geom_pointrange(aes(x=N, y=predicted_N_mean, ymin=predicted_N_lower, ymax=predicted_N_upper
                      ),
                   fill='grey50', color='grey70', shape=21
                  )+
  geom_abline(slope=1, intercept = 0, color='orange', size=1)+
  theme_minimal()+
  labs(title = 'Observed vs Predicted Check', x='Observed population count', y='Predicted population')
  
  
  
  
  
  
  