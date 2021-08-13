// This script contains the Stan code for the Bayesian model 1
// Model 1: Population count as a normal distribution 

data{
  
  int<lower=0> n; // number of microcensus clusters
  
  int<lower=0> population[n]; // count of people
  
  vector<lower=0>[n] area; // settled area
  
  int<lower=0> ncov; // number of covariates
  matrix[n, ncov] cov; // covariates
  
  int<lower=0> type[n]; // settlement type
  int<lower=0> ntype; // number of settlement types
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
    
  // reparametrization of hierarchical intercept
  vector[ntype] u_alpha_t;
  real alpha;
  real<lower=0> nu_alpha; 
  
  // slope
  vector[ncov] beta; 
  
  // variance
  real<lower=0> sigma; 
}

transformed parameters{
  vector[n] pop_density_mean;
  
    // reparametrization of hierarchical intercept
  vector[ntype] alpha_t; 

  alpha_t = alpha + u_alpha_t *  nu_alpha;
  
  pop_density_mean = alpha_t[type] + cov * beta;
  
}
model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  pop_density ~ lognormal( pop_density_mean, sigma );
  
  // hierarchical intercept by settlement
  u_alpha_t ~ std_normal();
  alpha ~ normal(0, 15);
  nu_alpha ~ uniform(0, 5);
  
  // slope
  beta ~ normal(0, 15);
  
  // variance
  sigma ~ uniform(0, 5);
}

generated quantities{
  
  int<lower=-1> population_hat[n];
  real<lower=-1> density_hat[n];
  
  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha_t[type[idx]] + cov[idx,] * beta, sigma );
    
    if(density_hat[idx] * area[idx]<1e+09){
      population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
    } else {
      population_hat[idx] = -1;
    }
  }
  
}



