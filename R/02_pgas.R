pgas <- function(M, N, K, TT,
                 y, yz, Za,
                 par_prior,
                 par_inits) {
  # Initialize data containers
  T <- TT
  w <- numeric(N)
  X <- matrix(0, nrow = M, ncol = T)

  sig_sq_xa <- numeric(M)
  phi_xa    <- numeric(M)
  bet_xa    <- matrix(0, nrow = length(par_inits[[1]][[3]]), ncol = M)

  regs       <- matrix(0, nrow = T - 1, ncol = ncol(Za) + 1)
  Za         <- as.matrix(Za)
  regs[, -1] <- Za[2:T, ]
  #  Initialize parameters
  sig_sq_xa[1] <- par_inits[[1]][[1]]
  phi_xa[1]    <- par_inits[[1]][[2]]
  bet_xa[, 1]  <- par_inits[[1]][[3]]
  #  Initialize priors
  prior_a     <- par_prior[1]
  prior_b     <- par_prior[2]
  # Initialize states by running a PF
  cpfOut <- cBPF_as(y = y, yz = yz, Za = Za,
                    N = N, TT = T, K = K,
                    sig_sq_xa = sig_sq_xa[1],
                    phi_xa = phi_xa[1],
                    bet_xa = bet_xa[, 1, drop = F],
                    x_r = X[1, ])
  w      <- cpfOut[[2]][, T]
  b      <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
  X[1, ] <- cpfOut[[1]][b, ]
  # Run MCMC loop
  for (m in 2:M) {
    how_long(m, M)
    # Run GIBBS PART
    err_sig_sq_x <- X[m - 1, 2:T] - f(xa_tt = X[m - 1, 1:(T - 1)],
                                      za = Za[2:T, , drop = F],
                                      phi_xa = phi_xa[m - 1],
                                      bet_xa = bet_xa[, m - 1])
    sig_sq_xa[m]  <- 1/rgamma(n = 1, prior_a + (T - 1)/2,
                             prior_b + crossprod(err_sig_sq_x)/2)
    # err_bet_sq_y <- y^2 * exp(-X[m - 1,])
    # phi_xa[m]     <- phi_xa[m - 1]
    # bet_xa[, m]   <- bet_xa[, m - 1]
    # sig_sq_xa[m]  <- true_sigSQ_x
    regs[, 1]    <- X[m - 1, 1:(T - 1)]
    x_lhs        <- X[m - 1, 2:T]
    Omega_xa     <- solve(crossprod(regs, regs)/sig_sq_xa[m] + 1)
    mu_xa        <- Omega_xa %*% (crossprod(regs, x_lhs)/sig_sq_xa[m])
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_xa[m]     <- beta_xa[1]  # true_phi_x
    bet_xa[, m]   <- beta_xa[-1] # true_bet_x
    while (near(abs(phi_xa[m]), 1, tol = 0.01) | abs(phi_xa[m]) > 1) {
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_xa[m]     <- beta_xa[1]  # true_phi_x
    bet_xa[, m]   <- beta_xa[-1] # true_bet_x
    }
    #
    # R <- chol(sigma, pivot = TRUE)
    # R[, order(attr(R, "pivot"))]
    # retval <- matrix(rnorm(n * ncol(sigma)),nrow = n,byrow =!pre0.9_9994)%*%R
    # retval <- sweep(retval, 2, mean, "+")
    #
    # Run CPF-AS PART
    cpfOut <- cBPF_as(y = y, yz = yz, Za = Za,
                      N = N, TT = T, K = K,
                      sig_sq_xa = sig_sq_xa[m],
                      phi_xa = phi_xa[m],
                      bet_xa = bet_xa[, m, drop = F],
                      x_r = X[m - 1,])
    w      <- cpfOut[[2]][, T]
    # draw b
    b <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
    X[m, ] <- cpfOut[[1]][b, ]
  }
  return(list(sigma_sq_x = sig_sq_xa,
              phi_xa = phi_xa,
              bet_xa = bet_xa,
              xtraj = X))
}
