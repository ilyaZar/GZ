generate_data <- function(T, K, num_incs,
                          par_true,
                          x_levels,
                          x_sd) {
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

  dim_reg_a <- length(bet_xa)
  dim_reg_b <- length(bet_xb)
  dim_reg_p <- length(bet_xp)
  dim_reg_q <- length(bet_xq)

  za <- matrix(rnorm(T*length(bet_xa)), nrow = T, ncol = dim_reg_a)
  # za[, 1] <- 1
  par_level_adjust <- za[, -dim_reg_a, drop = FALSE] %*% bet_xa[-dim_reg_a]
  par_level_adjust <- x_levels[1] * (1 - phi_xa) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_xa[dim_reg_a]
  za[, dim_reg_a]  <- par_level_adjust

  xinit <- 0 # x_levels[1]
  xa[1] <- f(x_tt = xinit, z = za[1, ], phi_x = phi_xa, bet_x = bet_xa)
  xa[1] <- xa[1] + sqrt(sig_sq_xa)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      xa[t + 1] <- f(x_tt = xa[t], z = za[t + 1, ],
                     phi_x = phi_xa, bet_x = bet_xa)
      xa[t + 1] <- xa[t + 1] + sqrt(sig_sq_xa)*rnorm(n = 1)
    }
  }
  xa <- exp(xa)
  zb <- matrix(rnorm(T*length(bet_xb)), nrow = T, ncol = dim_reg_b)
  # zb[, 1] <- 1
  par_level_adjust <- zb[, -dim_reg_b, drop = FALSE] %*% bet_xb[-dim_reg_b]
  par_level_adjust <- x_levels[2] * (1 - phi_xb) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_xb[dim_reg_b]
  zb[, dim_reg_b]  <- par_level_adjust

  xinit <- x_levels[2]
  xb[1] <- f(x_tt = xinit, z = zb[1, ], phi_x = phi_xb, bet_x = bet_xb)
  xb[1] <- xb[1] + sqrt(sig_sq_xb)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      xb[t + 1] <- f(x_tt = xb[t], z = zb[t + 1, ],
                     phi_x = phi_xb, bet_x = bet_xb)
      xb[t + 1] <- xb[t + 1] + sqrt(sig_sq_xb)*rnorm(n = 1)
    }
  }
  if (any(xb <= 1)) {
    stop("xb out of range")
  }
  # xb <- rep(150, times = T)
  zp <- matrix(rnorm(T*length(bet_xp)), nrow = T, ncol = dim_reg_p)
  # zp[, 1] <- 1
  par_level_adjust <- zp[, -dim_reg_p, drop = FALSE] %*% bet_xp[-dim_reg_p]
  par_level_adjust <- x_levels[3] * (1 - phi_xp) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_xp[dim_reg_p]
  zp[, dim_reg_p]  <- par_level_adjust

  xinit <- x_levels[3]
  xp[1] <- f(x_tt = xinit, z = zp[1, ], phi_x = phi_xp, bet_x = bet_xp)
  xp[1] <- xp[1] + sqrt(sig_sq_xp)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      xp[t + 1] <- f(x_tt = xp[t], z = zp[t + 1, ],
                     phi_x = phi_xp, bet_x = bet_xp)
      xp[t + 1] <- xp[t + 1] + sqrt(sig_sq_xp)*rnorm(n = 1)
    }
  }
  xp <- exp(xp)
  # xp <- rep(2.5, times = T)
  zq <- matrix(rnorm(T*length(bet_xq)), nrow = T, ncol = dim_reg_q)
  # zq[, 1] <- 1
  par_level_adjust <- zq[, -dim_reg_q, drop = FALSE] %*% bet_xq[-dim_reg_q]
  par_level_adjust <- x_levels[4] * (1 - phi_xq) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_xq[dim_reg_q]
  zq[, dim_reg_q]  <- par_level_adjust

  xinit <- x_levels[4]
  xq[1] <- f(x_tt = xinit, z = zq[1, ], phi_x = phi_xq, bet_x = bet_xq)
  xq[1] <- xq[1] + sqrt(sig_sq_xq)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      xq[t + 1] <- f(x_tt = xq[t], z = zq[t + 1, ],
                     phi_x = phi_xq, bet_x = bet_xq)
      xq[t + 1] <- xq[t + 1] + sqrt(sig_sq_xq)*rnorm(n = 1)
    }
  }
  # xq <- exp(xq)
  # xq <- rep(3.5, times = T)
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
  return(list(list(xa, xb, xp, xq), yraw, yz, za)) # , zb
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
