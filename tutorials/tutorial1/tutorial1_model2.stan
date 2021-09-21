// This script contains the Stan code for the Bayesian model 2
// Model 2: Population count as a lognormal distribution 

data{
  
  int<lower=0> n; // number of microcensus clusters
  
  int<lower=0> population[n]; // count of people
  
  vector<lower=0>[n] area; // settled area
  
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
  // intercept
  real mu; 
  
  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  pop_density ~ lognormal( mu, sigma );
  
  // mean
  mu ~ normal(5, 4);
  
  // variance
  sigma ~ uniform(0, 4);
}

generated quantities{
  
  int<lower=0> population_hat[n];
  real<lower=0> density_hat[n];
  
  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( mu, sigma );
    population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
  } 
}



