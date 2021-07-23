// This script contains the Stan code for the Bayesian model 1
// Model 1bis: Population count as a normal distribution with integrated predictions

data{
  
  int<lower=0> n; // number of microcensus clusters

  real<lower=0> population[n]; // count of people

}

parameters{
  
  // intercept
  real alpha; 
  
  // variance
  real<lower=0> sigma; 
}

model{
  
  // population totals
  population ~ normal( alpha, sigma );
  

  // intercept
  alpha ~ normal(0, 5000);

  // variance
  sigma ~ uniform(0, 50000);
}

generated quantities{
  
   real population_hat[n];

   for(idx in 1:n){
     population_hat[idx] = normal_rng( alpha, sigma );

   }
  
}
