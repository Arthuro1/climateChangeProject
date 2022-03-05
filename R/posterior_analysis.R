exportPattern("^[[:alpha:]]+")
# Normal regression model 3
# -> Formulate posterior
# -> Sample from posterior
# -> Check convergence
# -> Posterior predictive checks
# -> Inference
library(rstanarm)

posterior_analysis <- function(dependent, regressors, data.size = 120){

  posterior = stan_glm(as.formula(paste0(dependent, " ~ ", regressors)), data = climateset)

  regressor_seq = seq(from = -2, to = 2, length.out = data.size)

  coefs = coef(posterior)

  dependent_sim = sapply(1:data.size, function(i){
    rnorm(1e3,
          coefs[1] + coefs[2]*regressor_seq[i], posterior$covmat[2,2])
  })

  dependent_post_mean = apply(dependent_sim, MARGIN = 2, mean)

  plot(regressor_seq, dependent_post_mean, type = "l")

  #post = stan_glm(temperaturData_us ~ co2Data_us)
}
