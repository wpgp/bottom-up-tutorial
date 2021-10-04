###LOAD WP LOGO##
htmltools::img(src = knitr::image_uri("./assets/pic/320px-UNFPA_logo.svg.png"),
               alt = 'logo', style = 'position:absolute; top:60px; right:0; padding:20px; width: 25%; height: auto')


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

## 
## install.packages('raster')
## install.packages('RColorBrewer')
## install.packages('sf')
## install.packages('tmap')

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
pars <- c('mu','sigma', 'density_hat', 'population_hat')

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

warmup <- 250

fit_wrong <- rstan::stan(file = file.path('tutorials/tutorial4/tutorial1_model2_wrong.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

traceplot(fit_wrong, c('mu', 'sigma'))

# mcmc
fit <- rstan::stan(file = file.path('tutorials/tutorial1/tutorial1_model2.stan'), 
                          data = stan_data,
                          iter = warmup + iter, 
                          chains = chains,
                          warmup = warmup, 
                          pars = pars,
                          seed = seed)

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

set.seed(2004)
# sample observations
train_size <-  round(nrow(data)*0.7)
train_idx <- sample(1:nrow(data), size = train_size, replace=F)

# build train datasets
train_data <- data[train_idx,]

# build test datasets
test_data <- data[-train_idx,]

## data{

## 

##   int<lower=0> n_train; // number of microcensus clusters in training set

##   int<lower=0> n_test; // number of microcensus clusters in predicting set

## 
##   int<lower=0> population_train[n_train]; // count of people

## 

##   vector<lower=0>[n_train] area_train; // settled area in training

##   vector<lower=0>[n_test] area_test; // settled area in testing

## }

## 
## parameters{

##   // population density

##   vector<lower=0>[n_train] pop_density_train;

## 

##   //intercept

##   real mu;

## 
##   // variance

##   real<lower=0> sigma;

## }

## 
## model{

## 

##   // population totals

##   population_train ~ poisson(pop_density_train .* area_train);

##   pop_density_train ~ lognormal( mu, sigma );

## 

##   //  intercept

##   mu ~ normal(5, 4);

## 

##   // variance

##   sigma ~ uniform(0, 4);

## }

## 
## generated quantities{

## 

##   int<lower=-0> population_hat[n_test];

##   real<lower=0> density_hat[n_test];

## 
##   for(idx in 1:n_test){

##     density_hat[idx] = lognormal_rng( mu, sigma );

##     population_hat[idx] = poisson_rng(density_hat[idx] * area_test[idx]);

##   }

## }


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

tutorial3_model2_fit <- readRDS("tutorials/tutorial3/tutorial3_model2_fit.rds")

## library(raster)
## wd_path <- 'Z:/Projects/WP517763_GRID3/Working/NGA/ed/bottom-up-tutorial/'
## 
## # function that loads the raster and transforms it into an array
## getRasterValues <- function(x){
##   return( raster(paste0(wd_path,x))[])
## }
## 
## # get a list of the rasters
## rasterlist <- list.files(wd_path,
##                          pattern='.tif', full.names = F)
## # apply function to raster list
## raster_df <- as_tibble(sapply(rasterlist, getRasterValues))

## raster_df$gridcell_id <- 1:nrow(raster_df)
## 
## a <- raster_df %>% filter(mastergrid.tif==1) %>% dplyr::select(starts_with('settlement'), gridcell_id)
## sett_type <- 1:6
## a <- a %>%
##     rowwise() %>%
##     mutate(
##       settlementType=sett_type[which.max(c_across(starts_with('settlement')))])
## raster_df <- left_join(raster_df %>% dplyr::select(-starts_with('settlementarea')),
##                        a %>% dplyr::select(gridcell_id, settlementType))
## 
## colnames(raster_df) <- str_remove(colnames(raster_df), '.tif')
## 
## raster_df <- raster_df %>%
##   mutate(mastergrid = ifelse(is.na(x1), 0, mastergrid),
##          mastergrid = ifelse(settlementType==6, 0, mastergrid))
## 
## write_csv(raster_df %>% filter(mastergrid==1)%>% head(n=10), 'wd/raster_predict_ex.csv')

raster_df <- read.csv('wd/raster_predict_ex.csv')

settled <- raster_df %>% filter(mastergrid==1)
settled %>% head() %>% kbl() %>% kable_minimal()

## #load scaling factor
## scale_factor <- read_csv('tutorials/data/scale_factor.csv')
## 
## scaled_covs <- settled %>%
##   # subset covs column
##   dplyr::select(starts_with('x'), gridcell_id) %>%
## 
##   # convert table to long format
##   pivot_longer(-gridcell_id, names_to='cov', values_to = 'value') %>%
## 
##   # add scaling factor
##   left_join(scale_factor) %>%
## 
##   # scale covs
##   mutate(value_scaled= (value-cov_mean)/cov_sd) %>%
##   dplyr::select(gridcell_id, cov, value_scaled) %>%
## 
##   # convert table back to wide format
##   pivot_wider(names_from = cov, values_from = value_scaled)
## 
## # replace the covs with their scaled version
## raster_df <- raster_df %>% dplyr::select(-starts_with('x')) %>%
##   left_join(scaled_covs)
## 

## #extract betas
## beta_fixed <- t(rstan::extract(model_fit, pars='beta_fixed')$beta_fixed)
## beta_random <- t(rstan::extract(model_fit, pars='beta_random')$beta_random)
## #extract alphas
## alphas <- rstan::extract(model_fit, pars='alpha_t_r')$alpha_t_r
## #extract sigmas
## sigmas <-  rstan::extract(model_fit, pars='sigma')$sigma

## # extract covariates modelled with a fixed effect
## cov_fixed <- settled %>%
##   dplyr::select(x1:x3, x5:x6) %>%
##   as.matrix()
## 
## cov_fixed <- cov_fixed %*% beta_fixed

## beta_random <- as_tibble(beta_random)
## # add settlement type to beta random
## beta_random$settlementType <- 1:5
## 
## # extract covariates modelled with a random effect
## cov_random <- settled %>%
##   # subset random covariate and settlement type
##   dplyr::select(settlementType,x4) %>%
##   # associate correct estimated beta_t
##   left_join(beta_random) %>%
##   # multiply cov by slope
##   mutate(
##     across(starts_with('V'), ~ .x*x4)
##   ) %>%
##   # keep only the estimates
##   dplyr::select(-settlementType, -x4) %>%
##   as.matrix()

## # subset alpha_t for region 8
## alpha_t_8 <- as_tibble(t(alphas[,,8]))
## # assign settlement type
## alpha_t_8$settlementType <- 1:5
## 
## alpha_predicted <- settled %>%
##   dplyr::select(settlementType) %>%
##   left_join(alpha_t_8) %>%
##   dplyr::select(-settlementType) %>%
##   as.matrix()

## # sum mu components
## mu_predicted <- alpha_predicted + cov_fixed + cov_random

## predictions <- as_tibble(mu_predicted) %>%
##   # add grid cell id and the settled area to mu
##   mutate(
##       gridcell_id= settled$gridcell_id,
##       settled_area = settled$settled_area
##     )  %>%
##   # long format
##   pivot_longer(
##     -c(gridcell_id, settled_area),
##     names_to = 'iterations', values_to = 'mu_predicted') %>%
##   mutate(
##     # add sigma iterations for every grid cell
##     sigma=rep(sigmas, nrow(mu_predicted)),
##     # draw density for a log normal
##     density_predicted = rlnorm(n(), mu_predicted, sigma),
##     # draw population count from a Poisson
##     population_predicted = rpois(n(), density_predicted*settled_area)
##     ) %>%
##   dplyr::select(-mu_predicted,-sigma, -density_predicted) %>%
##   # convert it back to grid cell x iterations matrix
##   pivot_wider(-c(iteration, population_predicted),
##       names_from = iteration,
##       values_from = population_predicted
##     )

predictions <-  readRDS('tutorials/data/gridded_pop_predicted.rds')

predictions[1:5, ] %>% dplyr::select(gridcell_id, everything()) %>% 
  kbl() %>% kable_minimal()  %>% scroll_box(width = "100%")

prediction_ex <- predictions %>% slice(1:5) %>% 
         pivot_longer(-gridcell_id, names_to = 'iterations', values_to = 'prediction') %>% 
  group_by(gridcell_id) %>% 
  mutate(
    mean_pop = paste0(gridcell_id, ' (mean=', round(mean(prediction)), ')')
  )


ggplotly(ggplot(prediction_ex,   aes(x=prediction, color=mean_pop)) +
  geom_density()+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x='Population predicted', color='\n\nGridcell id \n(mean population)')) %>%
  layout(margin=list(t=100))

library(raster)
raster_df <- readRDS('wd/gridcell_id.rds')

mean_prediction <- tibble( 
  mean_prediction = apply(predictions %>% dplyr::select(-gridcell_id), 1, mean)
  )

mean_prediction <- mean_prediction %>% 
  # add gridcell_id
  mutate(gridcell_id = predictions$gridcell_id) %>% 
  # join unsettled grid cell
  right_join(raster_df %>% 
               dplyr::select(gridcell_id)) %>% 
  # sort according to position in the raster
  arrange(gridcell_id)

wd_path <- 'Z:/Projects/WP517763_GRID3/Working/NGA/ed/bottom-up-tutorial/'
mastergrid <- raster(paste0(wd_path, 'mastergrid.tif'))

# create raster
mean_r <- mastergrid
# assign mean prediction 
mean_r[] <- mean_prediction$mean_prediction

library(RColorBrewer)
# create color ramp
cuts <- quantile(mean_prediction$mean_prediction,
                 p=seq(from=0, to=1, length.out=7), na.rm=T)
col <-  brewer.pal(n = 7, name = "YlOrRd")

#plot mean prediction
plot(mean_r, col=col)

# retrieve the 95% credible interval
ci_prediction <- apply(predictions %>% dplyr::select(-gridcell_id),1, quantile, p=c(0.025, 0.975))

ci_prediction <- as_tibble(t(ci_prediction)) %>% 
    # add gridcell_id
  mutate(gridcell_id = predictions$gridcell_id) %>% 
  # join unsettled grid cell
  right_join(raster_df %>% 
               dplyr::select(gridcell_id)) %>% 
  # sort according to position in the raster
  arrange(gridcell_id) %>% 
  mutate(
    mean_prediction = mean_prediction$mean_prediction,
    uncertainty = (`97.5%` - `2.5%`)/ mean_prediction
  )

# create uncertainty raster
uncertainty_r <- mastergrid
uncertainty_r[] <- ci_prediction$uncertainty

#plot uncertainty raster
cuts <- quantile(ci_prediction$uncertainty,
                 p=seq(from=0, to=1, length.out=7), na.rm=T)
col <-  brewer.pal(n = 7, name = "Blues")
plot(uncertainty_r, col=col)

library(sf)
library(tmap)
tmap_options(check.and.fix = TRUE)
city <- st_read(paste0(wd_path, 'study_city.gpkg'), quiet=T)

names(mean_r) <- 'Gridded population'
tmap_mode('view')
tm_shape(city)+
  tm_borders()+
  tm_shape(mean_r)+
  tm_raster()+
  tm_basemap('Esri.WorldGrayCanvas')

mean_city <- extract(mean_r, city, fun=sum, na.rm=TRUE, df=TRUE)
mean_city %>%  
  mutate(features = 'city', .after=ID) %>% 
  rename("Mean population prediction"=Gridded.population) %>% 
  kbl(caption='Mean population prediction for the city computed with the gridded population') %>% kable_minimal(full_width = F)

city_r <- rasterize(city, mean_r)

city_prediction <- raster_df %>% 
  # select grid cell id from raster df
  dplyr::select(gridcell_id) %>% 
  # add city dummy
  mutate(city = city_r[]) %>% 
  # keep grid cells inside the city
  filter(city==1) %>% 
  # join predictions
  left_join(predictions) %>% 
  # keep predictions
  dplyr::select(starts_with('V'))


city_prediction <- as_tibble(apply(city_prediction,2, sum, na.rm=T))

city_prediction_stats <- city_prediction %>% 
  summarise( 
    "Mean population" = round(mean(value)),
    "Upper bound" = round(quantile(value, p=0.975)),
    'Lower bound' = round(quantile(value, p=0.025)))

ggplot(city_prediction, aes(x=value))+
  geom_density(size=1, color='orange')+
  theme_minimal()+
    theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(y='', x='Predicted population for the city')+
  annotate("segment",x=city_prediction_stats$`Mean population`,
           xend=city_prediction_stats$`Mean population`, y=0, yend=0.000005, size=1)+
  annotate('text',x=city_prediction_stats$`Mean population`+5000, y=0.0000012, hjust = 0, fontface =2,
           label=paste0('Mean prediction: \n', city_prediction_stats$`Mean population`, ' people'))+
  annotate("segment",x=city_prediction_stats$`Upper bound`,
           xend=city_prediction_stats$`Upper bound`, y=0, yend=0.000005)+
  annotate('text',x=city_prediction_stats$`Upper bound`+5000, y=0.000001, hjust = 0,
           label=paste0('97.5% prediction \nbound: \n', city_prediction_stats$`Upper bound`, ' people'))+
    annotate("segment",x=city_prediction_stats$`Lower bound`,
           xend=city_prediction_stats$`Lower bound`, y=0, yend=0.000005)+
    annotate('text',x=city_prediction_stats$`Lower bound`+5000, y=0.000001, hjust = 0,
           label=paste0('2.5% prediction \nbound: \n', city_prediction_stats$`Lower bound`, ' people'))
