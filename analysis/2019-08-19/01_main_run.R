############################## PGAS for GZ model ###############################
rm(list = ls())
# source("./R/helper/00_helper_lib_load.R")
# source("./R/helper/00_helper_model_fcts.R")
# source("./R/helper/00_helper_simulation_data.R")
# source("./R/helper/99_helper_diagnostics.R")

# source("./R/helper/01_helper_cBPF_as.R")
# source("./R/helper/02_helper_pgas.R")
# source("./R/01_cBPF_as.R")
# source("./R/02_pgas.R")
sapply(paste0(getwd(),"/R/", list.files(paste0(getwd(),"/R/"), recursive = TRUE)), source , echo = FALSE)
# PGAS run ----------------------------------------------------------------
simulate_data <- T
init_at_true  <- T
pgas_run      <- T
if (simulate_data) {
  set.seed(139423) # set.seed(3) #
  source("./analysis/2019-08-19/00_settings_simulation_data.R")
  source("./analysis/2019-08-19/00_settings_simulation_init.R")
} else {
  source("./analysis/2019-08-19/00_settings_simulation_init.R")
}
if (pgas_run) {
  out_pgas <- kf_mcmc(MM = num_mcmc, TT = TT,
                      y = y_t,
                      Za = za_t, Zb = zb_t, Zp = zp_t, Zq = zq_t,
                      priors = c(prior_a, prior_b),
                      par_init = par_init,
                      par_true = true_vals,
                      VCM_Y = VCM_Y_true,
                      filtering = TRUE,
                      num_plots_states = 1)
} else {
  out_gibbs <- kf_mcmc(MM = num_mcmc, TT = TT,
                       y = y_t,
                       Za = za_t, Zb = zb_t, Zp = zp_t, Zq = zq_t,
                       priors = c(prior_a, prior_b),
                       par_init = par_init,
                       par_true = true_vals,
                       VCM_Y = VCM_Y_true,
                       filtering = FALSE,
                       num_plots_states = 1)
}
kf_jsd_all <- out_pgas$xtraj

# ID_state  <- seq(from = num_state, by = dim_X, length.out = num_jsd)
# state_current_analysis <- kf_jsd[ID_state, ]
# mean_kf_smooth_1 <- colMeans(state_current_analysis)
# View(rbind(xt[num_state, ], matrix(mean_kf_smooth_1, byrow = TRUE, nrow = 1)))
#
# par(mfrow = c(2, 2))
# for (t in 1:TT) {
#   for (i in 1:4) {
#     state_current_analysis <- kf_jsd_all[[i]]
#     means_states <- colMeans(state_current_analysis)
#     hist(state_current_analysis[, t], main = paste0("state No. ", t))
#     abline(v = means_states[t], col = "red")
#     abline(v = xa_t[t], col = "green")
#     Sys.sleep(1)
#   }
# }
test_traj_a  <- colMeans(kf_jsd_all[[1]])
test_traj_b  <- colMeans(kf_jsd_all[[2]])
test_traj_p  <- colMeans(kf_jsd_all[[3]])
test_traj_q  <- colMeans(kf_jsd_all[[4]])
monitor_pgas_states(states_drawn = cbind(test_traj_a, test_traj_b,
                                         test_traj_p, test_traj_q),
                    states_true  = cbind(xa_t, xb_t, xp_t, xq_t),
                    current = 1, total = 1, num_prints = 1)


source("./analysis/2019-08-19/99_analyse_convergence_run.R")
