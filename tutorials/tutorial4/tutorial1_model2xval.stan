// This script contains the Stan code for the Bayesian model 2 in tutorial 1
// Model 2: Poisson/Lognormal + cross-validation

data{
  
  int<lower=0> n_train; // number of microcensus clusters in training set
  int<lower=0> n_test; // number of microcensus clusters in predicting set

  int<lower=0> population_train[n_train]; // count of people
  
  vector<lower=0>[n_train] area_train; // settled area in training
  vector<lower=0>[n_test] area_test; // settled area in testing
}

parameters{
  // population density
  vector<lower=0>[n_train] pop_density_train;
  
  //intercept
  real mu;

  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population_train ~ poisson(pop_density_train .* area_train);
  pop_density_train ~ lognormal( mu, sigma );
  
  //  intercept
  mu ~ normal(5, 4);
  
  // variance
  sigma ~ uniform(0, 4);
}

generated quantities{
  
  int<lower=-0> population_hat[n_test];
  real<lower=0> density_hat[n_test];

  for(idx in 1:n_test){
    density_hat[idx] = lognormal_rng( mu, sigma );
    population_hat[idx] = poisson_rng(density_hat[idx] * area_test[idx]);
  }
}



