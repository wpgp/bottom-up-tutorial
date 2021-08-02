// This script contains the Stan code for the Bayesian model 5 of tutorial 2
// Model 5: Hierarchical alpha by settlement type , region, state local + cauchy prior

data{
  
  int<lower=0> n; // number of microcensus clusters
  int<lower=0> population[n]; // count of people
  vector<lower=0>[n] area; // settled area
  
  int<lower=1> ntype; // number of settlement types
  int<lower=1, upper=ntype> type[n]; // settlement type
  
  int<lower=1> nregion; //number of regions
  int<lower=1,upper=nregion> region[n]; // region
  
  int<lower=1> nstate[nregion]; //number of states
  int<lower=1,upper=max(nstate)> state[n]; // state
  
  int<lower=1> max_nlocal; // max local with data in one state
  int<lower=0> nlocal[nregion, max(nstate)]; // number of local per region, per state
  int<lower=1,upper=max_nlocal> local[n]; // local
  
}

parameters{
  // population density
  vector<lower=0>[n] pop_density;
  
  // hierarchical intercept by settlement, region, state, local
  real alpha;
  vector[ntype] alpha_t; 
  vector[nregion] alpha_t_r[ntype];
  vector[max(nstate)] alpha_t_r_s[ntype, nregion];
  vector[max_nlocal] alpha_t_r_s_l[ntype, nregion, max(nstate)]; 
  
  real<lower=0> nu_alpha;
  real<lower=0> nu_alpha_t;
  real<lower=0> nu_alpha_r;
  real<lower=0> nu_alpha_s;

  
  // variance 
  real<lower=0> sigma; 
}

transformed parameters{
  vector[n] pop_density_mean;
  
  for(idx in 1:n){
    pop_density_mean[idx] = alpha_t_r_s_l[type[idx], region[idx], state[idx], local[idx]];
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
    for(r in 1:nregion){
      alpha_t_r_s[t,r,1:nstate[r]] ~ normal(alpha_t_r[t,r], nu_alpha_r);
      for(s in 1:nstate[r]){
        alpha_t_r_s_l[t,r,s,1:nlocal[r,s]] ~ normal(alpha_t_r_s[t,r,s], nu_alpha_s);
      }
    }
  }
  
  
  nu_alpha ~ uniform(0, 100);
  nu_alpha_t ~ uniform(0, 100);
  nu_alpha_r ~ uniform(0, 100);
  nu_alpha_s ~ uniform(0, 100);

  
  // variance with Cauchy prior
  sigma ~ cauchy(0, 1);
}

generated quantities{
  
  int<lower=-1> population_hat[n];
  real<lower=0> density_hat[n];
  
  for(idx in 1:n){
    density_hat[idx] = lognormal_rng( alpha_t_r_s_l[type[idx], region[idx], state[idx], local[idx]], sigma );
    if(density_hat[idx] * area[idx]<1e+09){
      population_hat[idx] = poisson_rng(density_hat[idx] * area[idx]);
    } else {
      population_hat[idx] = -1;
    }
    
  }
  
}



