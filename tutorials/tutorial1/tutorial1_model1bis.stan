// This script contains the Stan code for the Bayesian model 1
// Model 1: Population count as a Poisson distribution 

data{
  int<lower=0> n; // number of microcensus clusters
  int<lower=0> population[n]; // count of people
}

parameters{
  // rate
  real<lower=0> lambda; 
}

model{
  // population totals
  population ~ poisson(lambda);

  // rate
  lambda ~ uniform(0, 3000);
}

generated quantities{
  int<lower=0> population_hat[n];
  
  for(idx in 1:n){
    population_hat[idx] = poisson_rng(lambda);
  } 
}

