# 1. Set up parameter values ----------------------------------------------
# I. xa_t process parameters:
true_sig_sq_xa <- 0      # True latent state process noise variance
true_phi_xa    <- 1      # True autoregressive parameter for states
true_bet_xa    <- c(-2.5, 3) # c(-2.5, 3, -1, 0.5) # c(2) #   True regressor coefficients for states
# II. xb_t process parameters:
true_sig_sq_xb <- 0
true_phi_xb    <- 1
true_bet_xb    <- c(2, -1)
# III. xp_t process parameters:
true_sig_sq_xp <- 0
true_phi_xp    <- 1
true_bet_xp    <- c(-3, 4)
# IV. xq_t process parameters:
true_sig_sq_xq <- 0
true_phi_xq    <- 1
true_bet_xq    <- c(4, -5)
# V. Merging true parameters
par_true <- list(list(true_sig_sq_xa, true_phi_xa, true_bet_xa),
                 list(true_sig_sq_xb, true_phi_xb, true_bet_xb),
                 list(true_sig_sq_xp, true_phi_xp, true_bet_xp),
                 list(true_sig_sq_xq, true_phi_xq, true_bet_xq))
# 2. Data settings --------------------------------------------------------
# TT <- 200          # Length of data record
# KK       <- 10     # Number of income classes - 1
# num_obs  <- 10e4   # Number of total individual incomes
par_levels <- c(1.5, 80, 1, 1.5) # c(1.5, 50, 2.5, 3.5)
# 3. Generate data --------------------------------------------------------
