
library(rstan)
library(tidyverse)
library(here)

options(mc.cores = parallel::detectCores()-1)
rstan_options(auto_write = TRUE)


chains <- 4
iter <- 500
warmup <- 300
seed <- 12345678

c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_light_trans <- c("#DCBCBC80")
c_dark_trans <- c("#8F272780")
c_green_trans <- c("#00FF0080")

# Descriptive statistics --------------------------------------------------


hist(pop)


# Fit a Poisson model -----------------------------------------------------

# Prior predictive check
lambda_simulated <- abs(rnorm(1000, 500, 50))
pop_simulated <- sapply(lambda_simulated, function(x) rpois(1, x))

hist(pop_simulated)

# fit the model
input_data <- list(
  n_obs =  nrow(observations),
  pop = observations$pop
)

pars <- c('lambda', 'pop_post_pred')
  
fit_poisson <- stan(
  file= here('tutorials', 'refresher','poisson.stan'),
  data= input_data,
  iter = iter + warmup,
  warmup = warmup,
  seed = seed,
  pars = pars
)

# extract model outputs
samples_poisson <- rstan::extract(fit_poisson)

# visualise convergence
traceplot(fit_poisson, pars = 'lambda')

# prior retrodective check
lambda <- tibble(
  posterior = samples$lambda,
  prior = abs(rnorm(chains*iter, 500, 50)),
  iter = 1:(chains*iter)
) %>% 
  pivot_longer(-iter, names_to = 'distribution')

ggplot(lambda, aes(x=value, fill=distribution))+
  geom_histogram()+
  theme_minimal()

# posterior predictive check

ggplot(samples_poisson$pop_post_pred)

pop_posterior <- as_tibble(t(samples_poisson$pop_post_pred))
pop_posterior$pop <- observations$pop
pop_posterior$pop_group <- cut(pop_posterior$pop, seq(min(pop_posterior$pop), max(pop_posterior$pop), 50)) 

pop_posterior_grouped <- pop_posterior %>% 
  group_by(pop_group) %>% 
  dplyr::select(-pop) %>% 
  group_map(function(x) quantile(unlist(x), probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))

a <- pop_posterior %>% 
  slice(1:100) %>% 
  select(-pop_group, -pop)
head(unlist(a))
b <- quantile(unlist(a), probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

b <- cut(a$pop, 500)
B <- 2000

obs_counts <- hist(input_data$pop, breaks=(0:(B + 1))-0.5, plot=FALSE)$counts

idx <- rep(0:B, each=2)
x <- sapply(1:length(idx), function(b) if(b %% 2 == 0) idx[b] + 0.5 else idx[b] - 0.5)
pad_obs <- do.call(cbind, lapply(idx, function(n) obs_counts[n + 1]))

counts <- sapply(1:2000, function(n) hist(samples$pop_post_pred[n,], breaks=(0:(B + 1))-0.5, plot=FALSE)$counts)
probs <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:(B + 1), function(b) quantile(counts[b,], probs=probs))
pad_cred <- do.call(cbind, lapply(idx, function(n) cred[1:9,n + 1]))

plot(1, type="n", main="Posterior Retrodictive Check",
     xlim=c(-0.5, B + 0.5), xlab="y",
     ylim=c(0, max(c(obs_counts, cred[9,]))), ylab="")

polygon(c(x, rev(x)), c(pad_cred[1,], rev(pad_cred[9,])),
        col = c_light, border = NA)
polygon(c(x, rev(x)), c(pad_cred[2,], rev(pad_cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(x, rev(x)), c(pad_cred[3,], rev(pad_cred[7,])),
        col = c_mid, border = NA)
polygon(c(x, rev(x)), c(pad_cred[4,], rev(pad_cred[6,])),
        col = c_mid_highlight, border = NA)
lines(x, pad_cred[5,], col=c_dark, lwd=2)

lines(x, pad_obs, col="white", lty=1, lw=2.5)
lines(x, pad_obs, col="black", lty=1, lw=2)

