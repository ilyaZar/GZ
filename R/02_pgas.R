pgas <- function(M, N, K, TT,
                 y, yz, Za, Zb, Zp, Zq,
                 par_prior,
                 par_inits,
                 traj_init,
                 filtering = TRUE) {
  # Initialize data containers
  T  <- TT
  w  <- numeric(N)
  Xa <- matrix(0, nrow = M, ncol = T)
  Xb <- matrix(0, nrow = M, ncol = T)
  dim_ba <- length(par_inits[[1]][[3]])
  dim_bb <- length(par_inits[[2]][[3]])

  sig_sq_xa <- numeric(M)
  phi_xa    <- numeric(M)
  bet_xa    <- matrix(0, nrow = dim_ba, ncol = M)
  sig_sq_xb <- numeric(M)
  phi_xb    <- numeric(M)
  bet_xb    <- matrix(0, nrow = dim_bb, ncol = M)

  regs_a       <- matrix(0, nrow = T - 1, ncol = ncol(Za) + 1)
  Za           <- as.matrix(Za)
  regs_a[, -1] <- Za[2:T, ]
  regs_b       <- matrix(0, nrow = T - 1, ncol = ncol(Zb) + 1)
  Zb           <- as.matrix(Zb)
  regs_b[, -1] <- Zb[2:T, ]
  #  Initialize parameters
  sig_sq_xa[1] <- par_inits[[1]][[1]]
  phi_xa[1]    <- par_inits[[1]][[2]]
  bet_xa[, 1]  <- par_inits[[1]][[3]]
  sig_sq_xb[1] <- par_inits[[2]][[1]]
  phi_xb[1]    <- par_inits[[2]][[2]]
  bet_xb[, 1]  <- par_inits[[2]][[3]]
  #  Initialize state values (1st conditioning trajectory)
  Xa[1, ] <- traj_init[1]
  Xb[1, ] <- traj_init[2]
  # monitor_states(states_drawn = cbind(Xa[1, ], Xb[1, ]),
  #                states_true = cbind(xa_t, xb_t))
  #  Initialize priors
  prior_a      <- par_prior[1]
  prior_b      <- par_prior[2]
  prior_VCM_xa <- diag(dim_ba + 1)/1000
  prior_VCM_xb <- diag(dim_bb + 1)/1000
  # Initialize states by running a PF
  cpfOut <- cBPF_as(y = y, yz = yz, Za = Za, Zb = Zb, Zp = Zp, Zq = Zq,
                    N = N, TT = T, K = K,
                    sig_sq_xa = sig_sq_xa[1],
                    phi_xa = phi_xa[1],
                    bet_xa = bet_xa[, 1, drop = F],
                    xa_r = Xa[1, ],
                    sig_sq_xb = sig_sq_xb[1],
                    phi_xb = phi_xb[1],
                    bet_xb = bet_xb[, 1, drop = F],
                    xb_r = Xb[1, ],
                    filtering = filtering)
  w       <- cpfOut[[1]][, T]
  b       <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
  Xa[1, ] <- cpfOut[[2]][b, ]
  Xb[1, ] <- cpfOut[[3]][b, ]
  # Run MCMC loop
  for (m in 2:M) {
    how_long(m, M, len = M)
    # Run GIBBS PART
    err_sig_sq_x <- Xa[m - 1, 2:T] - f(x_tt = Xa[m - 1, 1:(T - 1)],
                                      z = Za[2:T, , drop = F],
                                      phi_x = phi_xa[m - 1],
                                      bet_x = bet_xa[, m - 1])
    sig_sq_xa[m]  <- 1/rgamma(n = 1, prior_a + (T - 1)/2,
                             prior_b + crossprod(err_sig_sq_x)/2)
    regs_a[, 1]  <- Xa[m - 1, 1:(T - 1)]
    x_lhs        <- Xa[m - 1, 2:T]
    Omega_xa     <- solve(crossprod(regs_a, regs_a)/sig_sq_xa[m] + prior_VCM_xa)
    mu_xa        <- Omega_xa %*% (crossprod(regs_a, x_lhs)/sig_sq_xa[m])
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_xa[m]    <- beta_xa[1]
    bet_xa[, m]  <- beta_xa[-1]
    while (near(abs(phi_xa[m]), 1, tol = 0.01) | abs(phi_xa[m]) > 1) {
    beta_xa      <- rmvnorm(n = 1, mean = mu_xa, sigma = Omega_xa)
    phi_xa[m]    <- beta_xa[1]
    bet_xa[, m]  <- beta_xa[-1]
    }
    err_sig_sq_x <- Xb[m - 1, 2:T] - f(x_tt =  Xb[m - 1, 1:(T - 1)],
                                      z = Zb[2:T, , drop = F],
                                      phi_x = phi_xb[m - 1],
                                      bet_x = bet_xb[, m - 1])
    sig_sq_xb[m]  <- 1/rgamma(n = 1, prior_a + (T - 1)/2,
                              prior_b + crossprod(err_sig_sq_x)/2)
    regs_b[, 1]  <- Xb[m - 1, 1:(T - 1)]
    x_lhs        <- Xb[m - 1, 2:T]
    Omega_xb     <- solve(crossprod(regs_b, regs_b)/sig_sq_xb[m] + prior_VCM_xb)
    mu_xb        <- Omega_xb %*% (crossprod(regs_b, x_lhs)/sig_sq_xb[m])
    beta_xb      <- rmvnorm(n = 1, mean = mu_xb, sigma = Omega_xb)
    phi_xb[m]    <- beta_xb[1]
    bet_xb[, m]  <- beta_xb[-1]
    while (near(abs(phi_xb[m]), 1, tol = 0.01) | abs(phi_xb[m]) > 1) {
      beta_xb      <- rmvnorm(n = 1, mean = mu_xb, sigma = Omega_xb)
      phi_xb[m]    <- beta_xb[1]
      bet_xb[, m]  <- beta_xb[-1]
    }
    #
    # R <- chol(sigma, pivot = TRUE)
    # R[, order(attr(R, "pivot"))]
    # retval <- matrix(rnorm(n * ncol(sigma)),nrow = n,byrow =!pre0.9_9994)%*%R
    # retval <- sweep(retval, 2, mean, "+")
    #
    # Run CPF-AS PART
    cpfOut <- cBPF_as(y = y, yz = yz, Za = Za, Zb = Zb, Zp = Zp, Zq = Zq,
                       N = N, TT = T, K = K,
                      sig_sq_xa = sig_sq_xa[m],
                      phi_xa = phi_xa[m],
                      bet_xa = bet_xa[, m, drop = F],
                      xa_r = Xa[m - 1,],
                      sig_sq_xb = sig_sq_xb[m],
                      phi_xb = phi_xb[m],
                      bet_xb = bet_xb[, m, drop = F],
                      xb_r = Xb[m - 1,],
                      filtering = filtering)
    w      <- cpfOut[[1]][, T]
    # draw b
    b <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
    Xa[m, ] <- cpfOut[[2]][b, ]
    Xb[m, ] <- cpfOut[[3]][b, ]
  }
  return(list(sigma_sq_xa = sig_sq_xa,
              phi_xa = phi_xa,
              bet_xa = bet_xa,
              sigma_sq_xb = sig_sq_xb,
              phi_xb = phi_xb,
              bet_xb = bet_xb,
              xtraj  = list(Xa, Xb)))
}
