data {
  int<lower=0> n_obs; // Number of observations
  int pop[n_obs];       // population count
}

parameters {
  real<lower=0> lambda[n_obs];   
  real alpha_national;     
  real<lower=0> sigma;     
  
}

model {
  // Prior model
  alpha_national ~ normal(log(500), 0.1);
  sigma ~ normal(0.3, 0.1);
  
  // Observational model
  
  pop ~ poisson(lambda);
  lambda ~ lognormal(alpha_national, sigma);
  
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  int pop_post_pred[n_obs]; 
  real<lower=0> lambda_post_pred[n_obs];   
  
  
  for(obs in 1:n_obs) {
    lambda_post_pred[obs]  = lognormal_rng(alpha_national, sigma);
    pop_post_pred[obs] = poisson_rng(lambda_post_pred[obs] );
  }
}

