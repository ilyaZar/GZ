generate_data <- function(T, sig_sq_xa, phi_xa, bet_xa,
                          par_levels = c(1.5, 150, 2.5, 3.5)) {
  xa      <- rep(0, T)
  y       <- rep(0, T)
  dim_reg <- length(bet_xa)

  za <- matrix(rnorm(T*length(bet_xa)), nrow = T, ncol = dim_reg)
  za[, 1] <- 1
  par_level_adjust <- za[, -dim_reg] %*% bet_xa[-dim_reg]
  par_level_adjust <- par_levels[1] * (1 - phi_xa) - par_level_adjust
  par_level_adjust <- par_level_adjust/bet_xa[dim_reg]
  za[, dim_reg]    <- par_level_adjust

  xinit <- 0
  xa[1] <- f(xa_tt = xinit, za = za[1, ], phi_xa = phi_xa, bet_xa = bet_xa)
  xa[1] <- xa[1] + sqrt(sig_sq_xa)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      xa[t + 1] <- f(xa_tt = xa[t], za = za[t + 1, ],
                     phi_xa = phi_xa, bet_xa = bet_xa)
      xa[t + 1] <- xa[t + 1] + sqrt(sig_sq_xa)*rnorm(n = 1)
    }
    y[t] <- 123
    # y[t] <- sqrt(bet_sq_y*exp(xa[t]))*rnorm(1)
    # y[t] <- g(xa[t]) + sqrt(bet_sq_y)*rnorm(1)
  }
  return(list(xa, y, za))
}
