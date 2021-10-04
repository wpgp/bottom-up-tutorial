// This script contains the Stan code for the Bayesian model 3 of tutorial 2
// Model 3: Hierarchical alpha by settlement type and region

data{
  
  int<lower=0> n; // number of microcensus clusters
  int<lower=0> population[n]; // count of people
  vector<lower=0>[n] area; // settled area
  
  int<lower=1> ntype; // number of settlement types
  int<lower=1, upper=ntype> type[n]; // settlement type
  
  int<lower=1> nregion; //number of regions
  int<lower=1,upper=nregion> region[n]; // region
  
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
  // hierarchical intercept by settlement and region
  real alpha;
  
  vector[ntype] alpha_t; 
  vector[nregion] alpha_t_r[ntype];
  
  real<lower=0> nu_alpha;
  real<lower=0> nu_alpha_t;
  
  
  // variance
  real<lower=0> sigma; 
}

transformed parameters{
  vector[n] pop_density_median;
  
  for(idx in 1:n){
    pop_density_median[idx] = alpha_t_r[type[idx],region[idx]];
  }
  
}

model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  
  pop_density ~ lognormal(pop_density_median, sigma );
  
  // hierarchical intercept by settlement and region
  alpha ~ normal(5, 10);
  nu_alpha ~ uniform(0, 15);
  nu_alpha_t ~ uniform(0, 15);
  
  
  alpha_t ~ normal(alpha, nu_alpha);
  
  for(t in 1:ntype){
    alpha_t_r[t,] ~ normal(alpha_t[t], nu_alpha_t); 
  }
  
  // variance
  sigma ~ uniform(0, 10);
}

generated quantities{
  
  int<lower=0> population_hat[n];
  real<lower=0> density_hat[n];
  
  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha_t_r[type[idx], region[idx]], sigma );
    population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
  }
}



