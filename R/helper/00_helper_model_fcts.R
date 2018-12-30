################################################################################
######################## PGAS model functions  GZ model ########################
################################################################################
#
#
#
#
#
f <- function(x_tt, z, phi_x, bet_x) {
  # xt <- phi_x*xtt
  x_t <- phi_x*x_tt + z %*% bet_x
  # xt <- phi_x*xtt + 8*cos(1.2*t)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(x_t)
}
w_xa <- function(xa, y, yz, xb, xp, xq, K) {
  N   <- length(xa)
  zks <- matrix(rep(yz, times = N), nrow = N, byrow = TRUE)
  d <- zks/xb
  d <- d^exp(xa)
  d <- d/(1 + d)

  F_gb2 <- pbeta(q = d, shape1 = xp, shape2 = xq)
  F_gb2 <- cbind(F_gb2, rep(1, times = N))

  pi_prob <- F_gb2[, 2:(K + 1)] - F_gb2[, 1:K]
  pi_prob <- log(pi_prob)
  pi_prob <- t(pi_prob)*y
  w <- .colSums(pi_prob, m = K, n = N)

  if (sum(is.nan(pi_prob))) {
    return(-Inf)
  }
  return(w)
}
