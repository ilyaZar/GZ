# Set up parameter values and other settings
if (testrun) {
  num_particles <- 5   # Number of particles used in PGAS
  TT <- 10             # Length of data record
  num_mcmc <- 10       # Number of iterations in the MCMC samplers
  burnin   <- 0        # Number of interations to burn
  # ALTERNATIVELY: TT <- 100, mcmc <- 3000, burnin <- 1000
  # Generate data
  true_sigSQ_x <- 0.1  # True process noise variance
  true_betSQ_y <- 1    # True measurement noise variance
  true_phi_x   <- 0.5  # True autoregressive/correlation parameter for states
  true_bet_x   <- 0    # True constant/random effect parameter for states
  # Initialization for the parameters
  init_bet_sq_y <- true_betSQ_y
  init_sig_sq_x <- true_sigSQ_x
  init_phi_x    <- true_phi_x
  init_bet_x    <- true_bet_x
} else {
  num_particles <- 50  # Number of particles used in PGAS
  TT <- 1000           # Length of data record
  num_mcmc <- 3000     # Number of iterations in the MCMC samplers
  burnin   <- 1000     # Number of interations to burn
  # Generate data
  true_sigSQ_x <- 0.2  # True process noise variance
  true_betSQ_y <- 1    # True measurement noise variance
  true_phi_x   <- 0.5  # True autoregressive/correlation parameter for states
  true_bet_x   <- c(1,-2, 3, -4, 5) # True regressor coefficients for states
  # Initialization for the parameters
  init_bet_sq_y <- true_betSQ_y
  # init_sig_sq_x <- true_sigSQ_x
  # init_phi_x    <- true_phi_x
  # init_bet_x    <- true_bet_x

  init_sig_sq_x <- 100
  init_phi_x    <- -0.9
  init_bet_x    <- -sign(true_bet_x)*10
}
dataSim <- generate_data(sig_sq_xa = true_sigSQ_x, phi_xa = true_phi_x,
                         bet_xa = true_bet_x, T = TT)
xt <- dataSim[[1]]
yt <- dataSim[[2]]
zt <- dataSim[[3]]
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
