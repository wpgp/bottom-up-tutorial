// This script contains the Stan code for the Bayesian model 1
// Model 1: Population count as a normal distribution 

data{
  
  int<lower=0> n; // number of microcensus clusters

  real<lower=0> population[n]; // count of people

}

parameters{
  
  // intercept
  real alpha; 
  
  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population ~ normal( alpha, sigma );
  

  // intercept
  alpha ~ normal(0, 5000);

  // variance
  sigma ~ uniform(0, 1000);
}

