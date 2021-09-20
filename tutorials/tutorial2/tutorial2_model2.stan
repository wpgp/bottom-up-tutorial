// This script contains the Stan code for the Bayesian model 2 of tutorial 2
// Model 2: Hierarchical alpha by settlement type

data{
  
  int<lower=0> n; // number of microcensus clusters
  int<lower=0> population[n]; // count of people
  vector<lower=0>[n] area; // settled area
  
  int<lower=0> type[n]; // settlement type
  int<lower=0> ntype; // number of settlement types

}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
    
  // hierarchical intercept by settlement
  vector[ntype] alpha_t; 
  real alpha;
  real<lower=0> nu_alpha;
  
  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  pop_density ~ lognormal( alpha_t[type], sigma );

  // hierarchical intercept by settlement
  alpha_t ~ normal(alpha, nu_alpha);
  alpha ~ normal(5, 10);
  nu_alpha ~ uniform(0, 15);

  // variance
  sigma ~ uniform(0, 10);
}

generated quantities{
  
   int<lower=0> population_hat[n];
   real<lower=0> density_hat[n];

  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha_t[type[idx]], sigma );
    population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
  }
  
}



