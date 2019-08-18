rm(list = ls())
source("./R/00_helper_lib_load.R")
source("./R/helper_pgas/00_helper_model_fcts.R")
source("./R/helper_pgas/00_helper_simulation_data.R")
source("./R/helper_optim/00_helper_optim.R")
source("./R/helper_optim/01_my_optim.R")
# PGAS run ----------------------------------------------------------------
set.seed(123) # set.seed(3)
source("./analysis/2019-06-10/00_settings_simulation_data.R")
TT <- 240
KK_vec       <- c(5, 10)     # Number of income classes - 1
num_obs_vec  <- c(10e2, 10e3)  # Number of total individual incomes
num_sample_size <- length(num_obs_vec)
results_gmm_estimation <- rep(list(list()), times = TT)
results_gmm_estimation <- rep(list(results_gmm_estimation), times = num_sample_size)
results_gmm_estimation <- rep(list(results_gmm_estimation), times = length(KK_vec))
for (k in 1:length(KK_vec)) {
  KK <- KK_vec[k]
  for (n in 1:num_sample_size) {
    num_obs <- num_obs_vec[n]
    dataSim <- generate_data_DGP_2(par_true = par_true,
                                             T = TT,
                                             K = KK,
                                             num_incs = num_obs,
                                             x_levels = par_levels,
                                             seq_logs = c(T, T, T, T),
                                             seq_cept = c(F, F, F, F),
                                             old_regs = FALSE,
                                             plot_states = FALSE)
    y_t  <- dataSim[[1]]
    # yz_t <- dataSim[[2]]
    xa_t <- dataSim[[3]][[1]]
    # xb_t <- dataSim[[3]][[2]]
    xp_t <- dataSim[[3]][[3]]
    xq_t <- dataSim[[3]][[4]]

    ginis <- numeric(TT)
    for (t in 1:TT) {
      ginis[t] <- gini.gb2(xa_t[t], xp_t[t], xq_t[t])
    }
    keepers <- !is.na(ginis)
    ginis  <- ginis[keepers]
    y_raw_means <- rowMeans(dataSim[[5]])
    y_raw_means <- y_raw_means[keepers]
    y_t <- y_t[keepers, ]
    TT <- sum(keepers)
    results_gmm_estimation[[k]][[n]] <- rep(list(list()), times = TT)
    for (t in 1:TT) {
    results_gmm_estimation[[k]][[n]][[t]] <- my_optim(y_t[t,],
                                                      gini.e = ginis[t],
                                                      pc.inc = y_raw_means[t],
                                                      se.gmm = FALSE,
                                                      se.scale = FALSE,
                                                      N = num_obs#, #rescale = 1,
                                                      # gini = TRUE
                                                      )
    iter <- ((n - 1 + (k - 1)*num_sample_size)*TT + t)/(2*num_sample_size*TT)
    print(paste0("Compute for ", KK, " income classes and sample size of ",
                 num_obs, " at dataset No. ", t,": ",
                 round(iter*100, digits = 2), "%!"))
    }
  }
}
