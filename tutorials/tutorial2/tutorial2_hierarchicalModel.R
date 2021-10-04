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

## install.packages('bayesplot') # additional evaluations of stan models
## install.packages('plotly') # interactive plot



# 2 Hierarchy in the data ----

library(RColorBrewer)

# prepare data
data <- readxl::read_excel(here('tutorials/data/nga_demo_data.xls'))
data <- data %>% 
  mutate(
    id = as.character(1:nrow(data)),
    pop_density=N/A
  )

# plot population density per region
ggplot(data %>% 
         group_by(
           region
         ) %>% 
         mutate(mean_popDens = mean(pop_density)) %>% 
         ungroup(), aes(fill=mean_popDens, x=pop_density, y=as.factor(region)))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_stepsn( colours = brewer.pal(6, "YlOrRd"))+
  labs(fill='Mean \npopulation \ndensity', x='Population density', y='Region')



# plot population density per settlement type
ggplot(data %>% 
         group_by(
           type
         ) %>% 
         mutate(mean_popDens = mean(pop_density)) %>% 
         ungroup(), aes(fill=mean_popDens, x=pop_density, y=as.factor(type)))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_stepsn( colours = brewer.pal(6, "YlOrRd"))+
  labs(fill='Population density \n(mean)', x='', y='Settlement type')

library(plotly)
# Visualise grouping of the data
# create unique id for the nested admin level
data1 <- data %>% 
  mutate(state= paste0(state,region),
         local = paste0(state, local))

# create data for sunburst plot
d1 <- rbind(
  # first layer
  data1 %>% 
    group_by(type) %>% 
    summarise(n=n()) %>% 
    mutate(
      ids = paste0('settlement', type),
      labels = paste0('settlement <br>', type),
      parents = '') %>% 
    ungroup() %>% 
    select(ids,labels, parents,n),
  # second layer
  data1 %>% 
    group_by(type, region) %>% 
    summarise(n=n()) %>% 
    mutate(
      ids = paste('settlement', type, '-', 'region', region),
      labels = paste0('region ', region),
      parents = paste0('settlement', type))%>% 
    ungroup() %>% 
    select(ids,labels, parents,n),
  # third layer
  data1 %>% 
    group_by(type, region, state) %>% 
    summarise(n=n()) %>% 
    mutate(
      ids = paste('settlement', type, '-', 'region', region, '-', 'state', state, '-', 'region', region),
      labels = paste0('state ', state),
      parents = paste('settlement', type, '-', 'region', region))%>% 
    ungroup() %>% 
    select(ids,labels, parents,n),
  # fourth layer
  data1 %>% 
    group_by(type, region, state, local) %>% 
    summarise(n=n()) %>% 
    mutate(
      ids = paste('settlement', type, '-', 'region', region, '-', 'state', state,  '-', 'local', local),
      labels = paste0('local ', local),
      parents = paste('settlement', type, '-', 'region', region, '-', 'state', state, '-', 'region', region))%>% 
    ungroup() %>% 
    select(ids,labels, parents,n)
) %>% 
  mutate(
    hover= paste(ids, '\n sample size', n)
  )

plot_ly(d1, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', 
        hovertext=~hover, insidetextorientation='radial')

makeSunburst2layer <- function(data){
  layers <- rbind(
  # first layer
  data %>% 
    group_by(type) %>% 
    summarise(n=sum(!is.na(N))) %>% 
    mutate(
      ids = paste0('settlement', type),
      labels = paste0('settlement <br>', type),
      parents = '') %>% 
    ungroup() %>% 
    select(ids,labels, parents,n),
  # second layer
  data %>% 
    group_by(type, region) %>% 
    summarise(n=sum(!is.na(N))) %>% 
    mutate(
      ids = paste('settlement', type, '-', 'region', region),
      labels = paste0('region ', region),
      parents = paste0('settlement', type))%>% 
    ungroup() %>% 
    select(ids,labels, parents,n)) %>%
    mutate(
      hover= paste(ids, '\n sample size', n),
      color= ifelse(n==0, 'yellow','')
    )
  
 return(layers)
}

# create missing combinations
data1_complete <- data1 %>% 
  complete(region, nesting(type)) 


plot_ly() %>% 
   add_trace(data=makeSunburst2layer(data1), 
             ids = ~ids, labels = ~labels, parents = ~parents, 
             type = 'sunburst', 
             hovertext=~hover, marker= list(colors=~color),  
             insidetextorientation='radial',
             domain = list(column = 0)) %>% 
     add_trace(data=makeSunburst2layer(data1_complete), 
             ids = ~ids, labels = ~labels, parents = ~parents, 
             type = 'sunburst', 
             hovertext=~hover, marker= list(colors=~color),  
             insidetextorientation='radial',
             domain = list(column = 1))  %>%  
  layout(
      grid = list(columns =2, rows = 1),
      margin = list(l = 0, r = 0, b = 0, t = 0))





# 3. Hierarchical model by settlement type ----

# No-pooling
# prepare data for stan
stan_data_model1 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A,
  type = data$type,
  ntype= n_distinct(data$type))


# mcmc settings
chains <- 4
warmup <- 250
iter <- 500
seed <- 1789

# parameters to monitor
pars <- c('alpha_t','sigma', 'population_hat', 'density_hat')

# mcmc
fit1 <- rstan::stan(file = file.path('tutorial2_model1.stan'), 
                   data = stan_data_model1,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)


# plot estimated parameter
stan_plot(fit1, pars='alpha_t', fill_color='orange')+
  # add alpha from tutorial 1
  geom_vline(xintercept=4.755109, size=1.5, linetype=2)+
  annotate('text', x=5, y=5.7, label="alpha estimated \nin tutorial 1")

# write function to extract posterior predictions and summarise them in a table
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
    summarise(across(everything(), list(mean=~mean(.), 
                                        upper=~quantile(., probs=0.975), 
                                        lower=~quantile(., probs=0.025)))) %>% 
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

# plot posterior predictions
ggplot(
  rbind(
    getPopPredictions(fit1) %>% 
      mutate(type='Population count'),
    getPopPredictions(fit1, estimate = 'density_hat', obs='pop_density') %>% 
      mutate(type='Population density')
  )  %>% 
         mutate(type= factor(type, levels=c('Population density', 'Population count')))) +
  geom_pointrange(aes(x=reference, y=predicted_mean, ymin=predicted_lower, ymax=predicted_upper
                      ),
                   fill='grey50', color='grey70', shape=21
                  )+
  geom_abline(slope=1, intercept = 0, color='orange', size=1)+
  theme_minimal()+
  labs(title = '', x='Observations', y='Predictions')+ 
  facet_wrap(.~type, scales = 'free')




# Partial pooling

pars <- c('alpha_t','alpha', 'nu_alpha', 'sigma', 'population_hat', 'density_hat')

# mcmc
fit2 <- rstan::stan(file = file.path('tutorial2_model2.stan'), 
                   data = stan_data_model1,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# extract parameters from both models
fit2_alpha_t <- summary(fit2, pars=c('alpha_t'))$summary
fit1_alpha_t <-  summary(fit1, pars=c('alpha_t'))$summary

# plot parameters
data_plot <- rbind(
  as_tibble(fit1_alpha_t, rownames='param') %>% 
    mutate(model='No pooling'),
    as_tibble(fit2_alpha_t, rownames='param') %>% 
    mutate(model='Partial pooling')
) %>% 
  mutate(
    model = factor(model, levels=c('No pooling','Partial pooling')),
    param_ = paste(param,model),
    labels = ifelse(model=='Partial pooling', param, '')) %>% 
arrange(param_)

ggplot(data_plot, aes(mean,param_, color=model, fill=model,label=labels))+
  geom_point()+
  geom_linerange(aes(xmin=`2.5%`, xmax=`97.5%`))+
  theme_minimal()+
  labs(y='')+
    scale_y_discrete(labels=data_plot$labels)


# Complete pooling 

# load previous model for complete pooling
fit_tuto1_model2 <- readRDS('../tutorial1/tutorial1_model2_fit.rds')

# build comprehensive dataframe
comparison_df <- rbind(
 getPopPredictions(fit1) %>% 
   mutate(model='No pooling'),
 getPopPredictions(fit2) %>% 
   mutate(model='Partial pooling'),
 getPopPredictions(fit_tuto1_model2) %>% 
   mutate(model='Complete pooling'))


# compute goodness-of-fit metrics
comparison_df %>% group_by(model) %>% 
  summarise( `Bias`= mean(residual),
    `Inaccuracy` = mean(abs(residual)),
        `Imprecision` = sd(residual)
) %>%  kbl(caption = 'Goodness-of-fit metrics comparison between complete pooling, no pooling, and partial pooling') %>% kable_minimal()



# 4. Hierarchical model by settlement type and region ----

# prepare data for stan
stan_data_model3 <- list(
  population = data$N,
  n = nrow(data),
  area = data$A,
  type = data$type,
  ntype= n_distinct(data$type),
  region = data$region,
  nregion = n_distinct(data$region)
  )


pars <- c('alpha','alpha_t','alpha_t_r','nu_alpha', 'sigma','population_hat',  'density_hat')

# mcmc
fit3 <- rstan::stan(file = file.path('tutorial2_model3.stan'), 
                   data = stan_data_model3,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

traceplot(fit3, 'density_hat[2]', inc_warmup=T)

alpha_4_r <- paste0('alpha_t_r[4,', 1:11, ']')
alpha_1_r <- paste0('alpha_t_r[1,', 1:11, ']')

stan_plot(fit3, pars=c('alpha','alpha_t[1]','alpha_t[4]', alpha_1_r, alpha_4_r), 
          fill_color='orange')


# build comprehensive dataframe
comparison_df <- rbind(
  comparison_df,
 getPopPredictions(fit3) %>% 
   mutate(model='Hierarchical - settlement, region'))

# compute goodness-of-fit metrics
comparison_df %>% 
  mutate(model =ifelse(model=='Partial pooling', 'Hierarchical - settlement', model)) %>% 
  filter(grepl('Hierarchical', model))  %>% group_by(model) %>% 
  summarise( `Bias`= mean(residual),
    `Inaccuracy` = mean(abs(residual)),
        `Imprecision` = sd(residual)
) %>%  kbl(caption = 'Goodness-of-metrics comparison of the hierarchical models') %>% kable_minimal()

## # save model
## saveRDS(fit3, 'tutorial2_model3_fit.rds')
