################################################################################
######################## PGAS model functions  GZ model ########################
################################################################################
#
#
#
#
#
f <- function(xa_tt, za, phi_xa, bet_xa) {
  # xt <- phi_x*xtt
  xa_t <- phi_xa*xa_tt + za %*% bet_xa
  # xt <- phi_x*xtt + 8*cos(1.2*t)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(xa_t)
}
w_xa <- function(xa, y, yz, B, P, Q, K) {
  # N   <- length(xa)
  # w   <- numeric(N)
  # w   <- matrix(0, nrow = N, ncol = K)
  # for (i in 1:N) {
  #   d <- yz/B
  #   d <- d^exp(xa[i])
  #   d <- d/(1 + d)
  #
  #   F_gb2 <- pbeta(q = d, shape1 = P, shape2 = Q)
  #   F_gb2 <- c(F_gb2, 1)
  #
  #   pi_prob <- F_gb2[2:(K + 1)] - F_gb2[1:K]
  #   pi_prob <- log(pi_prob)
  #   pi_prob <- pi_prob*y
  #   pi_prob <- sum(pi_prob)
  #
  #   if (sum(is.nan(pi_prob))) {
  #     return(-Inf)
  #   }
  #
  #   w[i,] <- pi_prob
  # }
  #
  #
  #
  #
  #
  N   <- length(xa)
  zks <- matrix(rep(yz, times = N), nrow = N, byrow = TRUE)
  d <- zks/B
  d <- d^exp(xa)
  d <- d/(1 + d)

  F_gb2 <- pbeta(q = d, shape1 = P, shape2 = Q)
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
