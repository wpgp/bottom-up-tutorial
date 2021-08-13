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
  
    // fixed slope
  int<lower=0> ncov_fixed; // number of covariates -1
  matrix[n, ncov_fixed] cov_fixed; // covariates
  
  // random slope
  vector[n] cov_random;
    
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
  // hierarchical intercept by settlement, region, state, local
  real alpha;
  vector[ntype] alpha_t; 
  vector[nregion] alpha_t_r[ntype];

  
  real<lower=0> nu_alpha;
  real<lower=0> nu_alpha_t;

  // variance 
  real<lower=0> sigma; 
  
  // slope
  row_vector[ncov_fixed] beta_fixed; 
  vector[ntype] beta_random;

}

transformed parameters{
  vector[n] pop_density_mean;
  vector[n] beta;

  for(idx in 1:n){
    beta[idx] = sum( cov_fixed[idx,] .* beta_fixed) + cov_random[idx] * beta_random[type[idx]];
    pop_density_mean[idx] = alpha_t_r[type[idx], region[idx]] + beta[idx];
  }
  
}

model{
  
  // population totals
  population ~ poisson(pop_density .* area);
  
  pop_density ~ lognormal(pop_density_mean, sigma );
  
  // hierarchical intercept by settlement and region
  alpha ~ normal(0, 100);
  
  alpha_t ~ normal(alpha, nu_alpha);
  
  for(t in 1:ntype){
    alpha_t_r[t,] ~ normal(alpha_t[t], nu_alpha_t); 
    }
  
  nu_alpha ~ uniform(0, 100);
  nu_alpha_t ~ uniform(0, 100);
  
  //slope
  beta_fixed ~ normal(0,10);
  beta_random ~ normal(0,10);

  // variance
  sigma ~ uniform(0, 100);
}

generated quantities{
  
  int<lower=-1> population_hat[n];
  real<lower=0> density_hat[n];
  vector[n] beta_hat;

  
  for(idx in 1:n){
    beta_hat[idx] = sum( cov_fixed[idx,] .* beta_fixed) + cov_random[idx] * beta_random[type[idx]];
    density_hat[idx] = lognormal_rng( alpha_t_r[type[idx], region[idx]] + beta_hat[idx], sigma );
    if(density_hat[idx] * area[idx]<1e+09){
      population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
    } else {
      population_hat[idx] = -1;
    }
    
  }
  
}



