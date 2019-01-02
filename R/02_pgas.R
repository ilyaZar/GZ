pgas <- function(MM, N, KK, TT,
                 y, yz, Za, Zb, Zp, Zq,
                 par_prior,
                 par_inits,
                 traj_init,
                 filtering = TRUE) {
  # Initialize data containers
  w  <- numeric(N)
  Xa <- matrix(0, nrow = MM, ncol = TT)
  Xb <- matrix(0, nrow = MM, ncol = TT)
  Xp <- matrix(0, nrow = MM, ncol = TT)
  dim_ba <- length(par_inits[[1]][[3]])
  dim_bb <- length(par_inits[[2]][[3]])
  dim_bp <- length(par_inits[[3]][[3]])

  sig_sq_xa <- numeric(MM)
  phi_xa    <- numeric(MM)
  bet_xa    <- matrix(0, nrow = dim_ba, ncol = MM)
  sig_sq_xb <- numeric(MM)
  phi_xb    <- numeric(MM)
  bet_xb    <- matrix(0, nrow = dim_bb, ncol = MM)
  sig_sq_xp <- numeric(MM)
  phi_xp    <- numeric(MM)
  bet_xp    <- matrix(0, nrow = dim_bp, ncol = MM)

  regs_a       <- matrix(0, nrow = TT - 1, ncol = ncol(Za) + 1)
  Za           <- as.matrix(Za)
  regs_a[, -1] <- Za[2:TT, ]
  regs_b       <- matrix(0, nrow = TT - 1, ncol = ncol(Zb) + 1)
  Zb           <- as.matrix(Zb)
  regs_b[, -1] <- Zb[2:TT, ]
  regs_p       <- matrix(0, nrow = TT - 1, ncol = ncol(Zp) + 1)
  Zp           <- as.matrix(Zp)
  regs_p[, -1] <- Zp[2:TT, ]
  #  Initialize parameters
  sig_sq_xa[1] <- par_inits[[1]][[1]]
  phi_xa[1]    <- par_inits[[1]][[2]]
  bet_xa[, 1]  <- par_inits[[1]][[3]]
  sig_sq_xb[1] <- par_inits[[2]][[1]]
  phi_xb[1]    <- par_inits[[2]][[2]]
  bet_xb[, 1]  <- par_inits[[2]][[3]]
  sig_sq_xp[1] <- par_inits[[3]][[1]]
  phi_xp[1]    <- par_inits[[3]][[2]]
  bet_xp[, 1]  <- par_inits[[3]][[3]]
  #  Initialize state values (1st conditioning trajectory)
  Xa[1, ] <- traj_init[1]
  Xb[1, ] <- traj_init[2]
  Xp[1, ] <- traj_init[3]
  monitor_states(states_drawn = cbind(exp(Xa[1, ]), Xb[1, ], exp(Xp[1, ])),
                 states_true  = cbind(xa_t, xb_t, xp_t),
                 current = 1, total = 1, num_prints = 1)
  #  Initialize priors
  prior_a      <- par_prior[1]
  prior_b      <- par_prior[2]
  prior_VCM_xa <- diag(dim_ba + 1)/1000
  prior_VCM_xb <- diag(dim_bb + 1)/1000
  prior_VCM_xp <- diag(dim_bp + 1)/1000
  # Initialize states by running a PF
  cpfOut <- cBPF_as(y = y, yz = yz, Za = Za, Zb = Zb, Zp = Zp, Zq = Zq,
                    N = N, TT = TT, KK = KK,
                    sig_sq_xa = sig_sq_xa[1],
                    phi_xa = phi_xa[1],
                    bet_xa = bet_xa[, 1, drop = F],
                    xa_r = Xa[1, ],
                    sig_sq_xb = sig_sq_xb[1],
                    phi_xb = phi_xb[1],
                    bet_xb = bet_xb[, 1, drop = F],
                    xb_r = Xb[1, ],
                    sig_sq_xp = sig_sq_xp[1],
                    phi_xp = phi_xp[1],
                    bet_xp = bet_xp[, 1, drop = F],
                    xp_r = Xp[1, ],
                    filtering = filtering)
  w       <- cpfOut[[1]][, TT]
  b       <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
  Xa[1, ] <- cpfOut[[2]][b, ]
  Xb[1, ] <- cpfOut[[3]][b, ]
  Xp[1, ] <- cpfOut[[4]][b, ]
  monitor_states(states_drawn = cbind(exp(Xa[1, ]), Xb[1, ], exp(Xp[1, ])),
                 states_true  = cbind(xa_t, xb_t, xp_t),
                 current = 1, total = 1, num_prints = 1)
  # Run MCMC loop
  for (m in 2:MM) {
    how_long(m, MM, len = MM)
    # Run GIBBS PART
    # 1. pars for xa_t process --------------------------------------------
    err_sig_sq_x <- Xa[m - 1, 2:TT] - f(x_tt = Xa[m - 1, 1:(TT - 1)],
                                      z = Za[2:TT, , drop = F],
                                      phi_x = phi_xa[m - 1],
                                      bet_x = bet_xa[, m - 1])
    sig_sq_xa[m]  <- 1/rgamma(n = 1, prior_a + (TT - 1)/2,
                             prior_b + crossprod(err_sig_sq_x)/2)
    regs_a[, 1]  <- Xa[m - 1, 1:(TT - 1)]
    x_lhs        <- Xa[m - 1, 2:TT]
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
    # 2. pars for xb_t process --------------------------------------------
    err_sig_sq_x <- Xb[m - 1, 2:TT] - f(x_tt =  Xb[m - 1, 1:(TT - 1)],
                                      z = Zb[2:TT, , drop = F],
                                      phi_x = phi_xb[m - 1],
                                      bet_x = bet_xb[, m - 1])
    sig_sq_xb[m]  <- 1/rgamma(n = 1, prior_a + (TT - 1)/2,
                              prior_b + crossprod(err_sig_sq_x)/2)
    regs_b[, 1]  <- Xb[m - 1, 1:(TT - 1)]
    x_lhs        <- Xb[m - 1, 2:TT]
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
    # 3. pars for xa_t process --------------------------------------------
    err_sig_sq_x <- Xp[m - 1, 2:TT] - f(x_tt =  Xp[m - 1, 1:(TT - 1)],
                                        z = Zp[2:TT, , drop = F],
                                        phi_x = phi_xp[m - 1],
                                        bet_x = bet_xp[, m - 1])
    sig_sq_xp[m]  <- 1/rgamma(n = 1, prior_a + (TT - 1)/2,
                              prior_b + crossprod(err_sig_sq_x)/2)
    regs_p[, 1]  <- Xp[m - 1, 1:(TT - 1)]
    x_lhs        <- Xp[m - 1, 2:TT]
    Omega_xp     <- solve(crossprod(regs_p, regs_p)/sig_sq_xp[m] + prior_VCM_xp)
    mu_xp        <- Omega_xp %*% (crossprod(regs_p, x_lhs)/sig_sq_xp[m])
    beta_xp      <- rmvnorm(n = 1, mean = mu_xp, sigma = Omega_xp)
    phi_xp[m]    <- beta_xp[1]
    bet_xp[, m]  <- beta_xp[-1]
    while (near(abs(phi_xp[m]), 1, tol = 0.01) | abs(phi_xp[m]) > 1) {
      beta_xp      <- rmvnorm(n = 1, mean = mu_xp, sigma = Omega_xp)
      phi_xp[m]    <- beta_xp[1]
      bet_xp[, m]  <- beta_xp[-1]
    }
    # Run CPF-AS PART
    cpfOut <- cBPF_as(y = y, yz = yz, Za = Za, Zb = Zb, Zp = Zp, Zq = Zq,
                      N = N, TT = TT, KK = KK,
                      sig_sq_xa = sig_sq_xa[m],
                      phi_xa = phi_xa[m],
                      bet_xa = bet_xa[, m, drop = F],
                      xa_r = Xa[m - 1,],
                      sig_sq_xb = sig_sq_xb[m],
                      phi_xb = phi_xb[m],
                      bet_xb = bet_xb[, m, drop = F],
                      xb_r = Xb[m - 1,],
                      sig_sq_xp = sig_sq_xp[m],
                      phi_xp = phi_xp[m],
                      bet_xp = bet_xp[, m, drop = F],
                      xp_r = Xp[m - 1,],
                      filtering = filtering)
    w      <- cpfOut[[1]][, TT]
    # draw b
    b <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
    Xa[m, ] <- cpfOut[[2]][b, ]
    Xb[m, ] <- cpfOut[[3]][b, ]
    Xp[m, ] <- cpfOut[[4]][b, ]
    monitor_states(states_drawn = cbind(exp(Xa[m, ]), Xb[m, ], exp(Xp[m, ])),
                   states_true = cbind(xa_t, xb_t, xp_t),
                   current = m, total = MM, num_prints = 10)
  }
  return(list(sigma_sq_xa = sig_sq_xa,
              phi_xa = phi_xa,
              bet_xa = bet_xa,
              sigma_sq_xb = sig_sq_xb,
              phi_xb = phi_xb,
              bet_xb = bet_xb,
              sigma_sq_xp = sig_sq_xp,
              phi_xp = phi_xp,
              bet_xp = bet_xp,
              xtraj  = list(Xa, Xb, Xp)))
}
