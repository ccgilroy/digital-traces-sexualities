#' Fit a Bayesian negative binomial model to national data 
#' of sexuality X relationship status X gender X age
#' 
# setup ----
library(dplyr)
library(readr)
library(here)
library(MASS, pos = 99)

library(rstanarm)
options(mc.cores = parallel::detectCores())

set.seed(123)

# load data
source("R/data_processing/relationship_data.R")

# select, rename, and adjust variables in data
# no substantive changes are made here
d <- 
  d_us_rel_sex_top4 %>%
  mutate(relationship_status = fct_drop(relationship_status)) %>%
  mutate(age = age_min) %>%
  select(gender, age, relationship_status, sexuality, estimate)

# fit model ----
# this model includes all two-way and three-way interactions
# it models age as a fifth-order polynomial
f <- estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^3

# fit MLE version of model as a quick test
# note: this also makes it easy to confirm that 
# a negative binomial model is much better than a poisson model
fit_mle <- glm.nb(formula = f, data = d)

# fit Bayesian model with weakly informative priors
fit_bayes <- stan_glm(formula = f, 
                      data = d, 
                      family = neg_binomial_2(link = "log"),
                      prior_intercept = normal(0, 10, autoscale = TRUE), 
                      prior = normal(0, 2.5, autoscale = TRUE), 
                      prior_aux = exponential(1, autoscale = TRUE), 
                      seed = 123, 
                      chains = 4, 
                      iter = 2000) 

# summaries and diagnostics ----
rstan::stan_rhat(fit_bayes) # good!
max(bayesplot::rhat(fit_bayes))
rstan::stan_ess(fit_bayes)
# check out some trace plots at random
rstan::stan_trace(fit_bayes, 
                  pars = sample(names(coef(fit_bayes)), 4))

# is it overdispersed? yes, yes it is.
rstan::stan_trace(fit_bayes, pars = "reciprocal_dispersion")
plot(fit_bayes, pars = "reciprocal_dispersion")

# they recommend k-fold cross-validation to assess fit
# seems like the model does badly at the ends of the data?
loo_fit <- loo(fit_bayes)
print(loo_fit)
plot(loo_fit, label_points = TRUE)
# loo_fit$pointwise
fit_bayes$data %>%
  tibble::rowid_to_column() %>%
  filter(rowid %in% loo::pareto_k_ids(loo_fit, threshold = .7))

fit_bayes$data %>%
  tibble::rowid_to_column() %>%
  filter(rowid %in% loo::pareto_k_ids(loo_fit, threshold = .5))

cv_fit <- kfold(fit_bayes, K = 10, save_fits = TRUE)

# compare to model with 2-way interactions only using 10-fold CV
# result: 3-way interactions definitely improve model fit
alt_model <- stan_glm(formula = estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2, 
                      data = d, 
                      family = neg_binomial_2(link = "log"),
                      prior_intercept = normal(0, 10), 
                      prior = normal(0, 2.5), 
                      prior_aux = exponential(1), 
                      seed = 123, 
                      chains = 4, 
                      iter = 2000) 

cv_fit_alt <- kfold(alt_model, K = 10, save_fits = TRUE)
loo_compare(cv_fit, cv_fit_alt)

# fit model without interactions
alt_model2 <- stan_glm(formula = estimate ~ sexuality + relationship_status + gender + poly(age, 5), 
                       data = d, 
                       family = neg_binomial_2(link = "log"),
                       prior_intercept = normal(0, 10), 
                       prior = normal(0, 2.5), 
                       prior_aux = exponential(1), 
                       seed = 123, 
                       chains = 4, 
                       iter = 2000) 
cv_fit_alt2 <- kfold(alt_model2, K = 10, save_fits = TRUE)
loo_compare(cv_fit, cv_fit_alt, cv_fit_alt2)

# save fit model object ----
write_rds(fit_bayes, file = here("output/intermediate/fit_bayes.rds"))
write_rds(cv_fit, file = here("output/intermediate/cv_fit_bayes.rds"))
