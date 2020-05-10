data {
  // Define all the data here
  int<lower=0> N; // number of observations
  int<lower=0> N_cat; // number of categories for intercept/slope
  vector[N] x; // explanatory variable
  vector[N] y; // response variable
  int<lower=0> cat[N]; // categorical variable
}
parameters {
  // Define parameters here
  real mu_alpha; // intercept mean
  real mu_beta; // slope mean
  vector[N_cat] alpha; // intercept
  vector[N_cat] beta; // Slope
  real<lower=0> sigma; // residual sd
  real<lower=0> sigma_alpha; // intercept sd
  real<lower=0> sigma_beta; // slope sd
}
model {
  // Write out the model likelihood here
  for(i in 1:N) {
    y[i] ~ normal(alpha[cat[i]] + beta[cat[i]] * x[i], sigma);
  }
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  sigma_alpha ~ cauchy(0, 1);
  sigma_beta ~ cauchy(0, 0.1);
  mu_alpha ~ normal(0, 10);
  mu_beta ~ normal(0, 1);
}
