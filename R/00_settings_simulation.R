# Set up parameter values and other settings
if (testrun) {
  num_particles <- 5   # Number of particles used in the conditional BPF
  TT <- 10             # Length of data record
  num_mcmc <- 10       # Number of iterations in the MCMC samplers
  burnin   <- 0        # Number of interations to burn
  # Generate data
  # I. xa_t process:
  true_sig_sq_xa <- 0.1      # True latent state process noise variance
  true_phi_xa    <- 0.5      # True autoregressive parameter for states
  true_bet_xa    <- c(-1, 2) # True regressor coefficients for states
  # Initialization for the parameters
  init_sig_sq_xa <- true_sig_sq_xa
  init_phi_xa    <- true_phi_xa
  init_bet_xa    <- true_bet_xa
  # II. xb_t process:
  true_sig_sq_xb <- 5
  true_phi_xb    <- 0.5
  true_bet_xb    <- c(2, -1)
  # Initialization for the parameters
  init_sig_sq_xb <- true_sig_sq_xb
  init_phi_xb    <- true_phi_xb
  init_bet_xb    <- true_bet_xb
  # III. xp_t process:
  true_sig_sq_xp <- 0.2
  true_phi_xp    <- 0.5
  true_bet_xp    <- c(-3, 4)
  # Initialization for the parameters
  init_sig_sq_xp <- true_sig_sq_xp
  init_phi_xp    <- true_phi_xp
  init_bet_xp    <- true_bet_xp
  # IV. xp_t process:
  true_sig_sq_xq <- 0.2
  true_phi_xq    <- 0.5
  true_bet_xq    <- c(4, -5)
  # Initialization for the parameters
  init_sig_sq_xq <- true_sig_sq_xq
  init_phi_xq    <- true_phi_xq
  init_bet_xq    <- true_bet_xq
  # V. Merging pars
  par_true <- list(list(true_sig_sq_xa, true_phi_xa, true_bet_xa),
                   list(true_sig_sq_xb, true_phi_xb, true_bet_xb),
                   list(true_sig_sq_xp, true_phi_xp, true_bet_xp),
                   list(true_sig_sq_xq, true_phi_xq, true_bet_xq))
  par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                   list(init_sig_sq_xb, init_phi_xb, init_bet_xb),
                   list(init_sig_sq_xp, init_phi_xp, init_bet_xp),
                   list(init_sig_sq_xq, init_phi_xq, init_bet_xq))
} else {
  num_particles <- 50  # Number of particles used in the conditional BPF
  TT <- 1000           # Length of data record
  num_mcmc <- 500     # Number of iterations in the MCMC samplers
  burnin   <- 0      # Number of interations to burn
  # Generate data
  # I. xa_t process:
  true_sig_sq_xa <- 0.2      # True latent state process noise variance
  true_phi_xa    <- 0.8      # True autoregressive parameter for states
  true_bet_xa    <- c(-2.5, 3) # True regressor coefficients for states
  # Initialization for the parameters
  init_sig_sq_xa <- true_sig_sq_xa
  init_phi_xa    <- true_phi_xa
  init_bet_xa    <- true_bet_xa
  # init_sig_sq_xa <- 1
  # init_phi_xa    <- -0.9
  # init_bet_xa    <- -sign(true_bet_xa)*0
  # II. xb_t process:
  true_sig_sq_xb <- 5
  true_phi_xb    <- 0.5
  true_bet_xb    <- c(2, -1)
  # Initialization for the parameters
  init_sig_sq_xb <- true_sig_sq_xb
  init_phi_xb    <- true_phi_xb
  init_bet_xb    <- true_bet_xb
  # init_sig_sq_xb <- 1
  # init_phi_xb    <- -0.9
  # init_bet_xb    <- -sign(true_bet_xb)*0.5
  # III. xp_t process:
  true_sig_sq_xp <- 0.2
  true_phi_xp    <- 0.5
  true_bet_xp    <- c(-3, 4)
  # Initialization for the parameters
  init_sig_sq_xp <- true_sig_sq_xp
  init_phi_xp    <- true_phi_xp
  init_bet_xp    <- true_bet_xp
  # init_sig_sq_xp <- 1
  # init_phi_xp    <- -0.9
  # init_bet_xp    <- -sign(true_bet_xp)*0.5
  # IV. xq_t process:
  true_sig_sq_xq <- 0.2
  true_phi_xq    <- 0.5
  true_bet_xq    <- c(4, -5)
  # Initialization for the parameters
  init_sig_sq_xq <- true_sig_sq_xq
  init_phi_xq    <- true_phi_xq
  init_bet_xq    <- true_bet_xq
  # init_sig_sq_xq <- 1
  # init_phi_xq    <- -0.9
  # init_bet_xq    <- -sign(true_bet_xq)*0.5
  # V. Merging pars
  par_true <- list(list(true_sig_sq_xa, true_phi_xa, true_bet_xa),
                   list(true_sig_sq_xb, true_phi_xb, true_bet_xb),
                   list(true_sig_sq_xp, true_phi_xp, true_bet_xp),
                   list(true_sig_sq_xq, true_phi_xq, true_bet_xq))
  par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                   list(init_sig_sq_xb, init_phi_xb, init_bet_xb),
                   list(init_sig_sq_xp, init_phi_xp, init_bet_xp),
                   list(init_sig_sq_xq, init_phi_xq, init_bet_xq))
}
KK       <- 10     # Number of income classes - 1
num_obs  <- 10e4   # Number of total individual incomes
par_levels <- c(1.5, 150, 2.5, 3.5)
dataSim <- generate_data(par_true = par_true,
                         T = TT,
                         K = KK,
                         num_incs = num_obs,
                         x_levels = par_levels,
                         x_sd = par_sd,
                         seq_exp  = c(T, F, T, F),
                         seq_cept = c(F, F, F, F))
y_raw <- dataSim[[1]]
yz_t  <- dataSim[[2]]
y_t   <- matrix(0, nrow = TT, ncol = KK)
for (t in 1:TT) {
  ncut <- cut(y_raw[t, ], breaks = yz_t[t, ])
  y_t[t, ] <- as.vector(table(ncut))
}
yz_t <- yz_t[, -(KK + 1)]
xa_t <- dataSim[[3]][[1]]
xb_t <- dataSim[[3]][[2]]
xp_t <- dataSim[[3]][[3]]
xq_t <- dataSim[[3]][[4]]
za_t <- dataSim[[4]][[1]]
zb_t <- dataSim[[4]][[2]]
zp_t <- dataSim[[4]][[3]]
zq_t <- dataSim[[4]][[4]]
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
# Testing parameter transformations
# par_levels <- c(1.5, 150, 2.5, 3.5)
# par_sd     <- c(0.5, 10, 1, 1)
# parameter_fct_log_norm(par_levels, par_sd)
# # revert_mu <- parameter_fct_log_norm(par_levels, par_sd)[[1]]
# # revert_sd <- parameter_fct_log_norm(par_levels, par_sd)[[2]]
# # parameter_fct_log_norm_test(revert_mu, sqrt(revert_sd))
# revert_mu <- log(c(1.5, 150, 2.5, 3.5))
# revert_sd <- sqrt(c(0.2, 0.00004, 0.2, 0.2))
# parameter_fct_log_norm_test(revert_mu, sqrt(revert_sd))
