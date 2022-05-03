data {
  int<lower=0> n_obs; // Number of observations
  int<lower=1> n_settlement; // Number of observations
  
  int pop[n_obs]; 
  int<lower=1, upper=n_settlement> obs_to_settlement[n_obs];// settlement index
}

parameters {
  real<lower=0> lambda[n_obs];   
  
  real<lower=0> alpha_national;  
  vector[n_settlement] alpha_settlement; 
  real<lower=0> u_alpha_settlement;
  
  vector<lower=0>[n_settlement] sigma_settlement; 
}


model {
  // Prior model
  alpha_national ~ normal(log(500), 0.1);
  
  alpha_settlement ~ normal(alpha_national, u_alpha_settlement);
  u_alpha_settlement ~ normal(0,1);
  
  sigma_settlement ~ normal(0.3, 0.1);

  // Observational model
  
  pop ~ poisson(lambda);
  lambda ~ lognormal(alpha_settlement[obs_to_settlement], sigma_settlement[obs_to_settlement]);
  
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  int pop_post_pred[n_obs]; 
  real<lower=0> lambda_post_pred[n_obs];   
  
  
  for(obs in 1:n_obs){
    lambda_post_pred[obs] = lognormal_rng(alpha_settlement[obs_to_settlement[obs]], 
                                          sigma_settlement[obs_to_settlement[obs]]);
    pop_post_pred[obs] = poisson_rng(lambda_post_pred[obs]);
  }
}

