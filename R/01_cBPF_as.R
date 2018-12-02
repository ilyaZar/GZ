cBPF_as <- function(y, N, Za, sig_sq_xa, phi_xa, bet_xa, x_r) {
  # DATA CONTAINERS
  T <- length(y)                      # time is from t = 0 to t = T (see below)
  x <- matrix(0, nrow = N, ncol = T)  # Particles
  a <- matrix(0, nrow = N, ncol = T)  # Ancestor indices
  w <- matrix(0, nrow = N, ncol = T)  # Weights

  # I. INITIALIZATION (t = 0)
  # Sampling initial condition from prior
  x[, 1]  <- rnorm(n = N, mean = Za[1, , drop = F] %*% bet_xa/(1 - phi_xa),
                   sd = sqrt(sig_sq_xa/(1 - phi_xa^2)))
  # corresponding weights
  w[, 1]  <- 1/N
  # II. FIRST PERIOD APPROXIMATION (t = 1)
  # resampling
  a[, 1]     <- sample.int(n = N, replace = TRUE, prob = w[, 1])
  # propagation
  eval_f  <- f(xa_tt = x[, 1], za = Za[1, , drop = F],
              phi_xa = phi_xa, bet_xa = bet_xa)
  x[, 1]  <- eval_f[a[, 1]] + sqrt(sig_sq_xa)*rnorm(N)
  # weighting
  w_log   <- -1/(2*bet_sq_y*exp(x[, 1]))*(y[1])^2 - 0.5 * x[, 1]
  w_max   <- max(w_log)
  w_tilde <- exp(w_log - w_max)
  w[, 1]  <- w_tilde/sum(w_tilde)
  # conditioning
  x[N, 1] <- x_r[1]

  # II. FOR t = 2,..,T
  for (t in 2:T) {
    # resampling
    a[, t]     <- sample.int(n = N, replace = TRUE, prob = w[, t - 1])
    # propagation
    eval_f     <- f(xa_tt = x[, t - 1], za = Za[t, , drop = F],
                    phi_xa = phi_xa, bet_xa = bet_xa)
    x[, t]     <- eval_f[a[, t]] + sqrt(sig_sq_xa)*rnorm(N)
    # conditioning
    x[N, t]    <- x_r[t]
    # ancestor sampling
    m          <- exp(-1/(2*sig_sq_xa)*(x_r[t] - eval_f)^2)
    w_as       <- w[, t - 1]*m
    w_as       <- w_as/sum(w_as)
    a[N, t]    <- sample.int(n = N, size = 1, replace = TRUE, prob = w_as)
    # weighting
    w_log      <- -1/(2*bet_sq_y*exp(x[, t]))*(y[t])^2 - 0.5 * x[, t]
    w_max      <- max(w_log)
    w_tilde    <- exp(w_log - w_max)
    w[, t]     <- w_tilde/sum(w_tilde)
  }
  # trajectories
  ind <- a[, T]
  for (t in (T - 1):1) {
    x[, t] <- x[ind, t]
    ind    <- a[ind, t]
  }
  return(list(x, w))
}
