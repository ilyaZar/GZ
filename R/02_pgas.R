pgas <- function(M, y, Z, par_prior, N, par_inits) {
  T <- length(y)

  sig_sq_x <- numeric(M)
  bet_sq_y <- numeric(M)
  phi_x    <- numeric(M)
  bet_x    <- matrix(0, nrow = length(par_inits[[4]]), ncol = M)

  regs       <- matrix(0, nrow = T - 1, ncol = ncol(Z) + 1)
  Z          <- as.matrix(Z)
  regs[, -1] <- Z[2:T, ]

  w <- numeric(N)
  X <- matrix(0, nrow = M, ncol = T)
  #  Initialize the parameters
  sig_sq_x[1] <- par_inits[[1]]
  bet_sq_y[1] <- par_inits[[2]]
  phi_x[1]    <- par_inits[[3]]
  bet_x[, 1]  <- par_inits[[4]]
  #  Initialize priors
  prior_a     <- par_prior[1]
  prior_b     <- par_prior[2]
  # Initialize the states by running a PF
  cpfOut <- cBPF_as(y = y, N = N, Z = Z,
                   sig_sq_x = sig_sq_x[1],
                   bet_sq_y = bet_sq_y[1],
                   phi_x = phi_x[1],
                   bet_x = bet_x[, 1, drop = F],
                   x_r = X[1, ])
  w      <- cpfOut[[2]][, T]
  b      <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
  X[1, ] <- cpfOut[[1]][b, ]
  # Run MCMC loop
  for (m in 2:M) {
    how_long(m, M)
    # Run GIBBS PART
    err_sig_sq_x <- X[m - 1, 2:T] - f(xtt = X[m - 1, 1:(T - 1)],
                                      z = Z[2:T, , drop = F],
                                      t = 1:(T - 1),
                                      phi_x = phi_x[m - 1],
                                      bet_x = bet_x[, m - 1])
    sig_sq_x[m]  <- 1/rgamma(n = 1, prior_a + (T - 1)/2,
                             prior_b + crossprod(err_sig_sq_x)/2)
    # err_bet_sq_y <- y^2 * exp(-X[m - 1,])
    # bet_sq_y[m]  <- 1/rgamma(n = 1, prior_a + T/2,
    #                          prior_b + sum(err_bet_sq_y)/2)
    # phi_x[m]     <- phi_x[m - 1]
    # bet_x[, m]   <- bet_x[, m - 1]
    # sig_sq_x[m]  <- true_sigSQ_x
    bet_sq_y[m]  <- true_betSQ_y
    regs[, 1]    <- X[m - 1, 1:(T - 1)]
    x_lhs        <- X[m - 1, 2:T]
    Omega_xa     <- solve(crossprod(regs, regs)/sig_sq_x[m] + 1)
    mu_xa        <- Omega_xa %*% (crossprod(regs, x_lhs)/sig_sq_x[m])
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_x[m]     <- beta_xa[1]  # true_phi_x
    bet_x[, m]   <- beta_xa[-1] # true_bet_x
    while (near(abs(phi_x[m]), 1, tol = 0.01) | abs(phi_x[m]) > 1) {
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_x[m]     <- beta_xa[1]  # true_phi_x
    bet_x[, m]   <- beta_xa[-1] # true_bet_x
    }
    #
    # R <- chol(sigma, pivot = TRUE)
    # R[, order(attr(R, "pivot"))]
    # retval <- matrix(rnorm(n * ncol(sigma)),nrow = n,byrow =!pre0.9_9994)%*%R
    # retval <- sweep(retval, 2, mean, "+")
    #
    # Run CPF-AS PART
    cpfOut <- cBPF_as(y = y, N = N, Z = Z,
                     sig_sq_x = sig_sq_x[m],
                     bet_sq_y = bet_sq_y[m],
                     phi_x = phi_x[m],
                     bet_x = bet_x[, m, drop = F],
                     x_r = X[m - 1,])
    w      <- cpfOut[[2]][, T]
    # draw b
    b <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
    X[m, ] <- cpfOut[[1]][b, ]
  }
  return(list(sigma_sq_x = sig_sq_x, beta_sq_y = bet_sq_y,
              phi_x = phi_x, bet_x = bet_x, xtraj = X))
}
