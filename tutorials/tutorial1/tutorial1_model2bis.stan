// This script contains the Stan code for the Bayesian model 1
// Model 1: Population count as a normal distribution 

data{
  
  int<lower=0> n; // number of microcensus clusters

  int<lower=0> population[n]; // count of people
  
  vector<lower=0>[n] area; // settled area

}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
    
  // intercept
  real<lower=0> alpha; 
  
  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  pop_density ~ lognormal( alpha, sigma );

  // intercept
  alpha ~ normal(0, 100);

  // variance
  sigma ~ uniform(0, 100);
}

generated quantities{
  
   int<lower=-1> population_hat[n];
   real<lower=0> density_hat[n];

  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha, sigma );
    
    if(density_hat[idx] * area[idx]<1e+09){
      population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
    } else {
      population_hat[idx] = -1;
    }
  }
  
}



