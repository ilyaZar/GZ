cBPF_as <- function(y, yz, Za, K, N, TT,
                    sig_sq_xa, phi_xa, bet_xa, xa_r,
                    sig_sq_xb, phi_xb, bet_xb, xb_r,
                    filtering = TRUE) {
  T <- TT
  if (!filtering) {
    xa <- matrix(rep(log(xa_t), times = N), nrow = N, ncol = T, byrow = TRUE)
    xb <- matrix(rep(xb_t, times = N), nrow = N, ncol = T, byrow = TRUE)
    w  <- matrix(1/N, nrow = N, ncol = T)
    return(list(w, xa, xb))
  }
  # DATA CONTAINERS
  # Particles for state processes x_a, x_b, x_p and x_q
  xa <- matrix(0, nrow = N, ncol = T)
  xb <- matrix(rep(xb_t, times = N), nrow = N, ncol = T, byrow = T)
  xp <- matrix(rep(xp_t, times = N), nrow = N, ncol = T, byrow = T)
  xq <- matrix(rep(xq_t, times = N), nrow = N, ncol = T, byrow = T)
  a  <- matrix(0, nrow = N, ncol = T)  # Ancestor indices
  w  <- matrix(0, nrow = N, ncol = T)  # Weights

  # I. INITIALIZATION (t = 0)
  # Sampling initial condition from prior
  xa[, 1] <- rnorm(n = N, mean = Za[1, , drop = F] %*% bet_xa/(1 - phi_xa),
                   sd = sqrt(sig_sq_xa/(1 - phi_xa^2)))
  # corresponding weights
  w[, 1]  <- 1/N
  # II. FIRST PERIOD APPROXIMATION (t = 1)
  # resampling
  a[, 1]  <- sample.int(n = N, replace = TRUE, prob = w[, 1])
  # propagation
  eval_f  <- f(x_tt = xa[, 1], z = Za[1, , drop = F],
               phi_x = phi_xa, bet_x = bet_xa)
  xa[, 1] <- eval_f[a[, 1]] + sqrt(sig_sq_xa)*rnorm(N)
  # weighting
  w_log   <- w_xa(xa = xa[, 1], y = y[1, ], yz = yz[1, ],
                  xb = xb[, 1], xp = xp[, 1], xq = xq[, 1], K = K)
  w_max   <- max(w_log)
  w_tilde <- exp(w_log - w_max)
  w[, 1]  <- w_tilde/sum(w_tilde)
  # conditioning
  xa[N, 1] <- xa_r[1]

  # II. FOR t = 2,..,T
  for (t in 2:T) {
    # resampling
    a[, t]     <- sample.int(n = N, replace = TRUE, prob = w[, t - 1])
    # propagation
    eval_f     <- f(x_tt = xa[, t - 1], z = Za[t, , drop = F],
                    phi_x = phi_xa, bet_x = bet_xa)
    xa[, t]    <- eval_f[a[, t]] + sqrt(sig_sq_xa)*rnorm(N)
    # conditioning
    xa[N, t]   <- xa_r[t]
    # ancestor sampling
    m          <- exp(-1/(2*sig_sq_xa)*(xa_r[t] - eval_f)^2)
    w_as       <- w[, t - 1]*m
    w_as       <- w_as/sum(w_as)
    a[N, t]    <- sample.int(n = N, size = 1, replace = TRUE, prob = w_as)
    # weighting
    w_log   <- w_xa(xa = xa[, t], y = y[t, ], yz = yz[t, ],
                    xb = xb[, t], xp = xp[, t], xq = xq[, t], K = K)
    w_max   <- max(w_log)
    w_tilde <- exp(w_log - w_max)
    w[, t]  <- w_tilde/sum(w_tilde)
  }
  # trajectories
  ind <- a[, T]
  for (t in (T - 1):1) {
    xa[, t] <- xa[ind, t]
    ind     <- a[ind, t]
  }
  return(list(w, xa, xb))
}
