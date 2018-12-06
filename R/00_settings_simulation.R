# Set up parameter values and other settings
if (testrun) {
  num_particles <- 5   # Number of particles used in PGAS
  TT <- 10             # Length of data record
  num_mcmc <- 10       # Number of iterations in the MCMC samplers
  burnin   <- 0        # Number of interations to burn
  # Generate data
  # I. xa_t process:
  true_sigSQ_xa <- 0.1      # True process noise variance
  true_phi_xa   <- 0.5      # True autoregressive parameter for states
  true_bet_xa   <- c(-1, 2) # True constant/random effect parameter for states
  # Initialization for the parameters
  init_sig_sq_xa <- true_sigSQ_xa
  init_phi_xa    <- true_phi_xa
  init_bet_xa    <- true_bet_xa
  # II. xb_t process:
  true_sigSQ_xb <- 0.2      # True process noise variance
  true_phi_xb   <- 0.7      # True autoregressive parameter for states
  true_bet_xb   <- c(2, -1) # True constant/random effect parameter for states
  # Initialization for the parameters
  init_sig_sq_xb <- true_sigSQ_xb
  init_phi_xb    <- true_phi_xb
  init_bet_xb    <- true_bet_xb
  # V. Merging pars
  par_true <- list(list(true_sigSQ_xa, true_phi_xa, true_bet_xa),
                    list(true_sigSQ_xb, true_phi_xb, true_bet_xb))
  par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                    list(init_sig_sq_xb, init_phi_xb, init_bet_xb))
} else {
  num_particles <- 50  # Number of particles used in PGAS
  TT <- 1000           # Length of data record
  num_mcmc <- 3000     # Number of iterations in the MCMC samplers
  burnin   <- 1000     # Number of interations to burn
  # Generate data
  # I. xa_t process:
  true_sigSQ_xa <- 0.2  # True process noise variance
  true_phi_xa   <- 0.5  # True autoregressive/correlation parameter for states
  true_bet_xa   <- c(1, -2, 3, -4, 5) # True regressor coefficients for states
  # Initialization for the parameters
  # init_sig_sq_xa <- true_sigSQ_xa
  # init_phi_xa    <- true_phi_xa
  # init_bet_xa    <- true_bet_xa
  init_sig_sq_xa <- 1
  init_phi_xa    <- -0.9
  init_bet_xa    <- -sign(true_bet_xa)*0.6
  # II. xb_t process:
  true_sigSQ_xb <- 0.2      # True process noise variance
  true_phi_xb   <- 0.7      # True autoregressive parameter for states
  true_bet_xb   <- c(2, -1) # True constant/random effect parameter for states
  # Initialization for the parameters
  init_sig_sq_xb <- true_sigSQ_xb
  init_phi_xb    <- true_phi_xb
  init_bet_xb    <- true_bet_xb
  # init_sig_sq_xb <- 1
  # init_phi_xb    <- -0.9
  # init_bet_xb    <- -sign(true_bet_xb)*0.5
  # V. Merging pars
  par_true <- list(list(true_sigSQ_xa, true_phi_xa, true_bet_xa),
                    list(true_sigSQ_xb, true_phi_xb, true_bet_xb))
  par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                    list(init_sig_sq_xb, init_phi_xb, init_bet_xb))
}
KK       <- 10     # Number of income classes - 1
num_obs  <- 10e4   # Number of total individual incomes
pars     <- c(1.5, 150, 2.5, 3.5)
dataSim <- generate_data(par_true = par_true,
                         T = TT,
                         K = KK,
                         num_incs = num_obs,
                         par_levels = log(pars))
x_t   <- dataSim[[1]]
y_raw <- dataSim[[2]]
yz_t  <- dataSim[[3]]
y_t   <- matrix(0, nrow = TT, ncol = KK)
for (t in 1:TT) {
  ncut <- cut(y_raw[t, ], breaks = yz_t[t, ])
  y_t[t, ] <- as.vector(table(ncut))
  # print(t)
}
yz_t <- yz_t[, -(KK + 1)]
z_t  <- dataSim[[4]]
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
