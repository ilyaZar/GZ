helper_as <- function(M, x) {
  apply(X = x,
        MARGIN = 1,
        function(x) {drop(crossprod(crossprod(M, x), x))})
}
cBPF_as <- function(y, yz, Za, Zb, Zp, Zq,
                    KK, N, TT,
                    sig_sq_xa, phi_xa, bet_xa, xa_r,
                    sig_sq_xb, phi_xb, bet_xb, xb_r,
                    filtering = TRUE) {
  if (!filtering) {
    xa <- matrix(rep(log(xa_t), times = N), nrow = N, ncol = TT, byrow = TRUE)
    xb <- matrix(rep(xb_t, times = N), nrow = N, ncol = TT, byrow = TRUE)
    xp <- matrix(rep(log(xp_t), times = N), nrow = N, ncol = TT, byrow = TRUE)
    w  <- matrix(1/N, nrow = N, ncol = TT)
    return(list(w, xa, xb, xp))
  }
  # DATA CONTAINERS
  # particles for state processes:
  xa <- matrix(0, nrow = N, ncol = TT)
  xb <- matrix(0, nrow = N, ncol = TT)
  xp <- matrix(rep(xp_t, times = N), nrow = N, ncol = TT, byrow = TRUE)
  xq <- matrix(rep(xq_t, times = N), nrow = N, ncol = TT, byrow = TRUE)
  # ancestors
  a  <- matrix(0, nrow = N, ncol = TT)
  # weights
  w  <- matrix(0, nrow = N, ncol = TT)

  # I. INITIALIZATION (t = 0)
  # Sampling initial condition from prior
  xa[, 1] <- rnorm(n = N, mean = Za[1, , drop = F] %*% bet_xa/(1 - phi_xa),
                   sd = sqrt(sig_sq_xa/(1 - phi_xa^2)))
  xb[, 1] <- rnorm(n = N, mean = Zb[1, , drop = F] %*% bet_xb/(1 - phi_xb),
                   sd = sqrt(sig_sq_xb/(1 - phi_xb^2)))
  # corresponding weights
  w[, 1]  <- 1/N
  # II. FIRST PERIOD APPROXIMATION (t = 1)
  # resampling
  a[, 1]  <- sample.int(n = N, replace = TRUE, prob = w[, 1])
  # propagation
  eval_fa <- f(x_tt = xa[, 1], z = Za[1, , drop = F],
               phi_x = phi_xa, bet_x = bet_xa)
  xa[, 1] <- eval_fa[a[, 1]] + sqrt(sig_sq_xa)*rnorm(N)
  eval_fb <- f(x_tt = xb[, 1], z = Zb[1, , drop = F],
               phi_x = phi_xb, bet_x = bet_xb)
  xb[, 1] <- eval_fb[a[, 1]] + sqrt(sig_sq_xb)*rnorm(N)
  # weighting
  w_log   <- w_xa(xa = xa[, 1], y = y[1, ], yz = yz[1, ],
                  xb = xb[, 1], xp = xp[, 1], xq = xq[, 1], KK = KK)
  w_max   <- max(w_log)
  w_tilde <- exp(w_log - w_max)
  w[, 1]  <- w_tilde/sum(w_tilde)
  # conditioning
  xa[N, 1] <- xa_r[1]
  xb[N, 1] <- xb_r[1]

  # II. FOR t = 2,..,T
  for (t in 2:TT) {
    # resampling
    a[, t]     <- sample.int(n = N, replace = TRUE, prob = w[, t - 1])
    # propagation
    eval_fa    <- f(x_tt = xa[, t - 1], z = Za[t, , drop = F],
                    phi_x = phi_xa, bet_x = bet_xa)
    xa[, t]    <- eval_fa[a[, t]] + sqrt(sig_sq_xa)*rnorm(N)
    eval_fb    <- f(x_tt = xb[, t - 1], z = Zb[t, , drop = F],
                    phi_x = phi_xb, bet_x = bet_xb)
    xb[, t]    <- eval_fb[a[, t]] + sqrt(sig_sq_xb)*rnorm(N)
    # conditioning
    xa[N, t]   <- xa_r[t]
    xb[N, t]   <- xb_r[t]
    # ancestor sampling
    m1         <- matrix(c(eval_fa - xa_r[t], eval_fb - xb_r[t]),
                         nrow = N, ncol = 2)
    m2         <- diag(c(sig_sq_xa^{-1}, sig_sq_xb^{-1}))
    m          <- -1/2 * helper_as(M = m2, x = m1)
    w_log_as   <- log(w[, t - 1]) + m
    w_max_as   <- max(w_log_as)
    w_tilde_as <- exp(w_log_as - w_max_as)
    w_as       <- w_tilde_as/sum(w_tilde_as)
    a[N, t]    <- sample.int(n = N, size = 1, replace = TRUE, prob = w_as)
    # weighting
    w_log   <- w_xa(xa = xa[, t], y = y[t, ], yz = yz[t, ],
                    xb = xb[, t], xp = xp[, t], xq = xq[, t], KK = KK)
    w_max   <- max(w_log)
    w_tilde <- exp(w_log - w_max)
    w[, t]  <- w_tilde/sum(w_tilde)
  }
  # trajectories
  ind <- a[, TT]
  for (t in (TT - 1):1) {
    xa[, t] <- xa[ind, t]
    xb[, t] <- xb[ind, t]
    ind     <- a[ind, t]
  }
  return(list(w, xa, xb))
}
