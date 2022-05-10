data {
  int<lower=0> n_obs; // Number of observations
  int pop[n_obs];       // population count
}

parameters {
  real<lower=0> alpha_national;     
}

model {
  // Prior model
  alpha_national ~ normal(500, 50);

  // Observational model
  
  pop ~ poisson(alpha_national);
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  int pop_post_pred[n_obs]; 
  
  for(obs in 1:n_obs)
    pop_post_pred[obs] = poisson_rng(alpha_national);
}

