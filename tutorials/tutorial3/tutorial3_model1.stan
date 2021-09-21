// This script contains the Stan code for the Bayesian model 1 of tutorial 3
// Model 1: Hierarchical alpha by settlement type , region + covariates

data{
  
  int<lower=0> n; // number of microcensus clusters
  int<lower=0> population[n]; // count of people
  vector<lower=0>[n] area; // settled area
  
  int<lower=1> ntype; // number of settlement types
  int<lower=1, upper=ntype> type[n]; // settlement type
  
  int<lower=1> nregion; //number of regions
  int<lower=1,upper=nregion> region[n]; // region
  
    // slope
  int<lower=0> ncov; // number of covariates
  matrix[n, ncov] cov; // covariates
    
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
  // hierarchical intercept by settlement, region
  real alpha;
  vector[ntype] alpha_t; 
  vector[nregion] alpha_t_r[ntype];
  real<lower=0> nu_alpha;
  real<lower=0> nu_alpha_t;

  // variance 
  real<lower=0> sigma; 
  
  // slope
  row_vector[ncov] beta; 
}

transformed parameters{
  vector[n] pop_density_mean;
  
  for(idx in 1:n){
    pop_density_mean[idx] = alpha_t_r[type[idx], region[idx]] + sum( cov[idx,] .* beta );
  }
  
}

model{
  // population totals
  population ~ poisson(pop_density .* area);
  
  pop_density ~ lognormal(pop_density_mean, sigma );
  
  // hierarchical intercept by settlement and region
  alpha ~ normal(5, 10);
  alpha_t ~ normal(alpha, nu_alpha);
  for(t in 1:ntype){
    alpha_t_r[t,] ~ normal(alpha_t[t], nu_alpha_t); 
    }
  
  nu_alpha ~ uniform(0, 15);
  nu_alpha_t ~ uniform(0, 15);
  
  //slope
  beta ~ normal(0,10);
  
  // variance
  sigma ~ uniform(0, 10);
}

generated quantities{
  
  int<lower=0> population_hat[n];
  real<lower=0> density_hat[n];
  
  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha_t_r[type[idx], region[idx]] + sum(cov[idx,] .* beta), sigma );
    population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
  }
}



