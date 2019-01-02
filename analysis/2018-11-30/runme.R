################################################################################
############################## PGAS for GZ model ###############################
################################################################################
# rm(list = ls())
source("./R/helper/00_helper_lib_load.R")
source("./R/helper/00_helper_general.R")
source("./R/helper/00_helper_model_fcts.R")
source("./R/helper/00_helper_simulation_data.R")
source("./R/helper/00_helper_diagnostics.R")

source("./R/01_cBPF_as.R")
source("./R/02_pgas.R")
# PGAS run ----------------------------------------------------------------
test          <- F
simulate_data <- T
init_at_true  <- F
if (test) {
  set.seed(123)
  source("./analysis/2018-11-30/00_settings_simulation_test.R")
} else if (simulate_data) {
  set.seed(139423)
  source("./analysis/2018-11-30/00_settings_simulation_run.R")
  source("./analysis/2018-11-30/00_settings_simulation_init.R")
} else {
  source("./analysis/2018-11-30/00_settings_simulation_init.R")
}
res <- pgas(N = num_particles, M = num_mcmc, K = KK, TT = TT,
            y = y_t, yz = yz_t, Za = za_t, Zb = zb_t, Zp = zp_t, Zq = zq_t,
            par_prior = c(prior_a, prior_b),
            par_inits = par_init,
            traj_init = deviate_states_init,
            filtering = TRUE)
source("./analysis/2018-11-30/99_analyse_convergence.R")
