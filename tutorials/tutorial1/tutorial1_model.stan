// Model for simulated data: y as normal distribution
data {
  int<lower=0> n; // number of observations
  real y[n]; // observations
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(mu, sigma);
  
  mu ~ normal(0,100);
  sigma ~ uniform(0,200);
}

