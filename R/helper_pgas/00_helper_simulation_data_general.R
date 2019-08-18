parameter_fct_log_norm <- function(exp_mu, exp_sd) {
  log_mu  <- log(exp_mu/sqrt( 1 + (exp_sd^2/exp_mu^2) ))
  log_var <- log(1 + exp_sd^2/exp_mu^2)
  return(list(log_mu, log_var))
}
parameter_fct_log_norm_test <- function(log_mu, log_sd) {
  exp_mu  <- exp(log_mu + log_sd^2/2)
  exp_var <- (exp(log_sd^2) - 1)*(exp(2*log_mu + log_sd^2))
  return(list(exp_mu, exp_var))
}
