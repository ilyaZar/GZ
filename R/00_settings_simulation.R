# Set up parameter values and other settings
if (testrun) {
  num_particles <- 5   # Number of particles used in PGAS
  TT <- 10             # Length of data record
  num_mcmc <- 10       # Number of iterations in the MCMC samplers
  burnin   <- 0        # Number of interations to burn
  # Generate data
  true_sigSQ_x <- 0.1  # True process noise variance
  true_phi_x   <- 0.5  # True autoregressive/correlation parameter for states
  true_bet_x   <- 0    # True constant/random effect parameter for states
  # Initialization for the parameters
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
KK       <- 10     # Number of income classes - 1
num_obs  <- 10e3   # Number of total individual incomes
pars     <- c(1.5, 150, 2.5, 3.5)
dataSim <- generate_data(sig_sq_xa = true_sigSQ_x,
                         phi_xa = true_phi_x,
                         bet_xa = true_bet_x,
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
z_t <- dataSim[[4]]
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
