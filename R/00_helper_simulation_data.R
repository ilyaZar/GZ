generate_data <- function(T, K, num_incs,
                          par_true,
                          x_levels,
                          x_sd,
                          seq_exp, seq_cept) {
  xa <- rep(0, T)
  xb <- rep(0, T)
  xp <- rep(0, T)
  xq <- rep(0, T)

  sig_sq_xa <- par_true[[1]][[1]]
  phi_xa    <- par_true[[1]][[2]]
  bet_xa    <- par_true[[1]][[3]]
  sig_sq_xb <- par_true[[2]][[1]]
  phi_xb    <- par_true[[2]][[2]]
  bet_xb    <- par_true[[2]][[3]]
  sig_sq_xp <- par_true[[3]][[1]]
  phi_xp    <- par_true[[3]][[2]]
  bet_xp    <- par_true[[3]][[3]]
  sig_sq_xq <- par_true[[4]][[1]]
  phi_xq    <- par_true[[4]][[2]]
  bet_xq    <- par_true[[4]][[3]]

  res_a <- generate_x_z(phi_x = phi_xa, sig_sq_x = sig_sq_xa, bet_x = bet_xa,
                        x_level = x_levels[1],
                        process_exp = seq_exp[1],
                        intercept = seq_cept[1],
                        x_init = F,
                        T = T)
  xa    <- res_a[[1]]
  za    <- res_a[[2]]
  res_b <- generate_x_z(phi_x = phi_xb, sig_sq_x = sig_sq_xb, bet_x = bet_xb,
                        x_level = x_levels[2],
                        process_exp = seq_exp[2],
                        intercept = seq_cept[2],
                        x_init = TRUE,
                        T = T)
  xb    <- res_b[[1]]
  zb    <- res_b[[2]]
  res_p <- generate_x_z(phi_x = phi_xp, sig_sq_x = sig_sq_xp, bet_x = bet_xp,
                        x_level = x_levels[3],
                        process_exp = seq_exp[3],
                        intercept = seq_cept[3],
                        x_init = TRUE,
                        T = T)
  xp    <- res_p[[1]]
  zp    <- res_p[[2]]
  res_q <- generate_x_z(phi_x = phi_xq, sig_sq_x = sig_sq_xq, bet_x = bet_xq,
                        x_level = x_levels[4],
                        process_exp = seq_exp[4],
                        intercept = seq_cept[4],
                        x_init = TRUE,
                        T = T)
  xq <- res_q[[1]]
  zq <- res_q[[2]]

  seq_prob <- rep(seq(from = 0, to = 1 - (1/K), length.out = K), each = T)
  yz <- matrix(qgb2(prob = seq_prob,
                    shape1 = xa,
                    scale  = xb,
                    shape2 = xp,
                    shape3 = xq),
               nrow = T, ncol = K)
  yz   <- cbind(yz, rep(Inf, times = T))
  yraw <- matrix(rgb2(n = num_incs*T,
                      shape1 = xa,
                      scale  = xb,
                      shape2 = xp,
                      shape3 = xq),
                 nrow = T, ncol = num_incs)
  return(list(yraw, yz, list(xa, xb, xp, xq), list(za, zb, zp, zq)))
}
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
generate_x_z <- function(phi_x, sig_sq_x, bet_x,
                         x_level,
                         process_exp,
                         intercept,
                         x_init,
                         T) {
  if (process_exp) {
    x_level <- log(x_level)
  }
  dim_reg <- length(bet_x)
  x <- rep(0, T)

  z <- matrix(rnorm(T*dim_reg), nrow = T, ncol = dim_reg)
  if (intercept) {
    z[, 1] <- 1
  }
  par_level_adjust <- z[, -dim_reg, drop = FALSE] %*% bet_x[-dim_reg]
  par_level_adjust <- x_level * (1 - phi_x) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_x[dim_reg]
  z[, dim_reg]     <- par_level_adjust

  if (x_init == TRUE) {
    xinit <- x_level
  } else {
    xinit <- 0
  }
  x[1] <- f(x_tt = xinit, z = z[1, ], phi_x = phi_x, bet_x = bet_x)
  x[1] <- x[1] + sqrt(sig_sq_x)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      x[t + 1] <- f(x_tt = x[t], z = z[t + 1, ],
                    phi_x = phi_x, bet_x = bet_x)
      x[t + 1] <- x[t + 1] + sqrt(sig_sq_x)*rnorm(n = 1)
    }
  }
  if (process_exp) {
    x <- exp(x)
  }
  if (sum(any(x <= 0)) & process_exp == FALSE) {
    stop("state process (xa_t, xb_t, xp_t or xq_t) out of range (not positive)")
  }
  return(list(x, z))
}
