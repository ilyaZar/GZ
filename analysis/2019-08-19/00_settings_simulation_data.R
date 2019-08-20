# 1. Set up parameter values ----------------------------------------------
# I. xa_t process parameters:
true_sig_sq_xa <- 0.2      # True latent state process noise variance
true_phi_xa    <- 0.8      # True autoregressive parameter for states
true_bet_xa    <- c(-0.5, 3) # c(-2.5, 3, -1, 0.5) # c(2) #   True regressor coefficients for states
# II. xb_t process parameters:
true_sig_sq_xb <- 0.1
true_phi_xb    <- 0.5
true_bet_xb    <- c(2, -1)
# III. xp_t process parameters:
true_sig_sq_xp <- 0.2
true_phi_xp    <- 0.5
true_bet_xp    <- c(-1, 4)
# IV. xq_t process parameters:
true_sig_sq_xq <- 0.2
true_phi_xq    <- 0.5
true_bet_xq    <- c(4, -1.5)
# V. Merging true parameters
par_true <- list(list(true_sig_sq_xa, true_phi_xa, true_bet_xa),
                 list(true_sig_sq_xb, true_phi_xb, true_bet_xb),
                 list(true_sig_sq_xp, true_phi_xp, true_bet_xp),
                 list(true_sig_sq_xq, true_phi_xq, true_bet_xq))
VCM_Y_true <- diag(rep(1, times = 4))
# 2. Data settings --------------------------------------------------------
TT <- 100          # Length of data record
KK       <- 10     # Number of income classes - 1
num_obs  <- 10e4   # Number of total individual incomes
par_levels <- c(2.5, 100, 2, 2.5) # c(1.5, 50, 2.5, 3.5)
# 3. Generate data --------------------------------------------------------
dataSim <- generate_data_lgssm_DGP_1(par_true = par_true,
                                     VCM_Y = VCM_Y_true,
                                     T = TT,
                                     K = KK,
                                     num_incs = num_obs,
                                     x_levels = par_levels,
                                     seq_logs = c(F, F, F, F),
                                     # seq_cept = c(F, F, F, F),
                                     seq_cept = c(T, T, T, T),
                                     old_regs = FALSE,
                                     plot_states = TRUE)
y_t  <- dataSim[[1]]
yz_t <- dataSim[[2]]
xa_t <- dataSim[[3]][[1]]
xb_t <- dataSim[[3]][[2]]
xp_t <- dataSim[[3]][[3]]
xq_t <- dataSim[[3]][[4]]
# xa_t2 <- dataSim[[3]][[1]]
# xb_t2 <- dataSim[[3]][[2]]
# xp_t2 <- dataSim[[3]][[3]]
# xq_t2 <- dataSim[[3]][[4]]
za_t <- dataSim[[4]][[1]]
zb_t <- dataSim[[4]][[2]]
zp_t <- dataSim[[4]][[3]]
zq_t <- dataSim[[4]][[4]]
# y_raw <- dataSim[[5]]
