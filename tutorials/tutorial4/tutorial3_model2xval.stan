// This script contains the Stan code for the Bayesian model 3
// Model 2: random effect in cov + cross-validation

data{
  
  int<lower=0> n_train; // number of microcensus clusters in training set
  int<lower=0> n_test; // number of microcensus clusters in predicting set

  int<lower=0> population_train[n_train]; // count of people
  
  vector<lower=0>[n_train] area_train; // settled area in training
  vector<lower=0>[n_test] area_test; // settled area in testing
  
    // fixed slope
  int<lower=0> ncov_fixed; // number of covariates -1
  matrix[n_train, ncov_fixed] cov_fixed_train; // covariates in training
  matrix[n_test, ncov_fixed] cov_fixed_test; // covariates in training
  
    // random slope
  vector[n_train] cov_random_train;
  vector[n_test] cov_random_test;

  int<lower=0> ntype; // number of settlement types
  int<lower=0> type_train[n_train]; // settlement type in train
  int<lower=0> type_test[n_test]; // settlement type in test
  
  int<lower=1> nregion; //number of regions
  int<lower=1,upper=nregion> region_train[n_train]; // region
  int<lower=1,upper=nregion> region_test[n_test]; // region

}

parameters{
  // population density
  vector<lower=0>[n_train] pop_density_train;
  
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
  vector[n_train] pop_density_train_mean;
  vector[n_train] beta_train;

  for(idx in 1:n_train){
    beta_train[idx] = sum( cov_fixed_train[idx,] .* beta_fixed) + cov_random_train[idx] * beta_random[type_train[idx]];
    pop_density_train_mean[idx] = alpha_t_r[type_train[idx], region_train[idx]] + beta_train[idx];
  }
  
  
}
model{
  
  // population totals
  population_train ~ poisson(pop_density_train .* area_train);
  pop_density_train ~ lognormal( pop_density_train_mean, sigma );
  
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
  
  int<lower=-1> population_hat[n_test];
  real<lower=-1> density_hat[n_test];
  vector[n_test] beta_hat;

  for(idx in 1:n_test){
    beta_hat[idx] = sum( cov_fixed_test[idx,] .* beta_fixed) + cov_random_test[idx] * beta_random[type_test[idx]];
    density_hat[idx] = lognormal_rng( alpha_t_r[type_test[idx], region_test[idx]] + beta_hat[idx], sigma );
    if(density_hat[idx] * area_test[idx]<1e+09){
      population_hat[idx] = poisson_rng(density_hat[idx] * area_test[idx]);
    } else {
      population_hat[idx] = -1;
    }
    
  }
  
}



