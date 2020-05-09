data {
  // Define all the data here
  int<lower=0> N; // number of observations
  vector[N] x; // explanatory variable
  vector[N] y; // response variable
}
parameters {
  // Define parameters here
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma; // residual sd
}
model {
  // Write out the model here
  y ~ normal(alpha + beta * x, sigma);
}
