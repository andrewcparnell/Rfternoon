# Learn the basics of rstan in < 1 hour!

# Start by loading in the package
# run install.packages('rstan', 'ggplot2') if the below doesn't work
library(rstan)
library(ggplot2)

# Run these two commands to make rstan run faster (and in parallel)
rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

# Workflow
# 1) Write a stan model in a separate file (named .stan)
# 2) Wrangle your data into the correct format, store it as a list with the same names as in the stan file
# 3) Create a stan_model object (this will also check for syntax errors)
# 4) Fit the model with either the optimizing (max likelihood) function or sampling (Bayes) functions
# 5) Play with the results

# Fit a linear regression -------------------------------------------------

# The .stan file - lin_reg.stan
# Open this up to see what's in it
# Different blocks for data, parameters and the model
# Every line ends in ;
# Uses // for comments
# Strongly typed

# Let's look at the data we're going to fit
head(mtcars)
# We're going to try and predict mpg wfrom wt

# Create a quick plot using ggplot (see 1_learn_ggplot2.R)
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point()

# Set it up in a list with the same names as the stan file
cars_data = list(N = nrow(mtcars),
                 x = mtcars$wt,
                 y = mtcars$mpg)

# Maximum likelihood version
stan_model_lr = stan_model('lin_reg.stan')
stan_run_lr_ml = optimizing(stan_model_lr,
                            data = cars_data)
print(stan_run_lr_ml)
# Not particularly friendly output - also no uncertainties

# Plot
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_abline(intercept = stan_run_lr_ml$par['alpha'],
              slope = stan_run_lr_ml$par['beta'])

# The full Bayesian way
stan_run_lr_bayes = sampling(stan_model_lr,
                             data = cars_data)
print(stan_run_lr_bayes)
plot(stan_run_lr_bayes) # Not always helpful if parameters on very different scales

# A more complicated model ------------------------------------------------
# Hierarchical regression -------------------------------------------------

# Earnings data - want to estimate log earnings from height based on age and ethnicity categories
earnings = read.csv('https://raw.githubusercontent.com/andrewcparnell/bhm_course/master/data/earnings.csv')
ggplot(earnings, aes(x = height_cm, y = y)) +
  geom_jitter(aes(colour = as.factor(eth))) +
  xlab('height (cm)') +
  ylab('log(Earnings ($))') +
  theme_bw()

# Consider a model where we estimate the slope and intercept for different ethnic groups
earnings_data_lr = list(N = nrow(earnings),
                        N_cat = 4,
                        x = earnings$height_cm,
                        y = earnings$y,
                        cat = earnings$eth)

# Look at the clever indexing in this model
earnings_model_lr = stan_model('hier_reg.stan')
earnings_run_lr_bayes = sampling(earnings_model_lr, 
                                 data = earnings_data_lr)
plot(earnings_run_lr_bayes) # Now have a intercept and slope for each
plot(earnings_run_lr_bayes, pars = 'beta') # Now have a intercept and slope for each
print(earnings_run_lr_bayes)

# The next task is to plot these estimated slopes and intercepts for each ethnic group
intercepts = get_posterior_mean(earnings_run_lr_bayes, pars = 'alpha')
slopes = get_posterior_mean(earnings_run_lr_bayes, pars = 'beta')
out = data.frame(eth = 1:4,
                 intercepts = intercepts[,'mean-all chains'],
                 slopes = slopes[,'mean-all chains'])

# Create the plot
ggplot(earnings, aes(x = height_cm, y = y)) +
  geom_jitter(aes(colour = as.factor(eth))) +
  xlab('height (cm)') +
  ylab('log(Earnings ($))') +
  theme_bw() + 
  facet_wrap(~ eth) +
  geom_abline(data = out, 
              aes(intercept = intercepts,
                  slope = slopes))

# And finally plot the different slopes to see if they are different
all_slopes = extract(earnings_run_lr_bayes, pars = 'beta')$beta
slopes_df = data.frame(
  eth = rep(1:4, each = nrow(all_slopes)),
  slopes = as.vector(all_slopes)
)
ggplot(slopes_df, aes(x = slopes)) +
  geom_histogram(bins = 30) +
  xlab('Estimated slope') + 
  theme_bw() + 
  facet_wrap(~ as.factor(eth))



# What else can I do? -----------------------------------------------------

# The Stan user guide has 100s of examples: https://mc-stan.org/users/documentation/
# Stan forums for help https://discourse.mc-stan.org
# ... but SO is usually better: https://stackoverflow.com/questions/tagged/rstan
# A useful book: https://www.elsevier.com/books/doing-bayesian-data-analysis/kruschke/978-0-12-405888-0

# Exercise ----------------------------------------------------------------

# Try and implement a new linear regression on the cars data above. This time predict the miles per gallon based on both weight (wt) and number of cylinders). If you get stuck have a look in the stan user guide for multiple linear regression. 
