data {
  int<lower=0> n_obs; // Number of observations
  int pop[n_obs];       // population count
}

parameters {
  real<lower=0> lambda;     
}

model {
  // Prior model
  lambda ~ normal(500, 50);

  // Observational model
  
  pop ~ poisson(lambda);
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  int pop_post_pred[n_obs]; 
  
  for(obs in 1:n_obs)
    pop_post_pred[obs] = poisson_rng(lambda);
}

