generate_data <- function(T, K, num_incs, sig_sq_xa,
                          phi_xa, bet_xa, par_levels) {
  xa      <- rep(0, T)
  # yt      <- matrix(0, nrow = T, ncol = num_incs)
  # yz      <- matrix(0, nrow = T, ncol = K)
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
    # yz[t, ] <- qgb2(prob = seq(from = 0, to = 1 - (1/K), length.out = K),
    #                 shape1 = xa[t],
    #                 scale = par_levels[2],
    #                 shape2 = par_levels[3], shape3 = par_levels[4])
    # yt[t, ] <- rgb2(n = num_incs, shape1 = xa[t],
    #                 scale = par_levels[2],
    #                 shape2 = par_levels[3],
    #                 shape3 = par_levels[4])
    # y[t] <- sqrt(bet_sq_y*exp(xa[t]))*rnorm(1)
    # y[t] <- g(xa[t]) + sqrt(bet_sq_y)*rnorm(1)
    # print(t)
  }
  xa <- exp(xa)
  seq_prob <- rep(seq(from = 0, to = 1 - (1/K), length.out = K), each = T)
  yz <- matrix(qgb2(prob = seq_prob,
                    shape1 = xa,
                    scale = par_levels[2],
                    shape2 = par_levels[3], shape3 = par_levels[4]),
               nrow = T, ncol = K)
  yz   <- cbind(yz, rep(Inf, times = T))
  yraw <- matrix(rgb2(n = num_incs*T, shape1 = xa,
                      scale = par_levels[2],
                      shape2 = par_levels[3],
                      shape3 = par_levels[4]),
                 nrow = T, ncol = num_incs)
  return(list(xa, yraw, yz, za))
}
