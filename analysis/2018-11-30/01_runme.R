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
simulate_data <- F
init_at_true  <- T
pgas_run      <- F
if (test) {
  set.seed(123)
  source("./analysis/2018-11-30/00_settings_simulation_test.R")
  source("./analysis/2018-11-30/00_settings_simulation_test_init.R")
  pgas_run <- TRUE
} else if (simulate_data) {
  set.seed(139423) # set.seed(3) #
  source("./analysis/2018-11-30/00_settings_simulation_run.R")
  source("./analysis/2018-11-30/00_settings_simulation_run_init.R")
} else {
  source("./analysis/2018-11-30/00_settings_simulation_run_init.R")
}
if (pgas_run) {
  out_pgas <- pgas(N = num_particles, MM = num_mcmc, KK = KK, TT = TT,
                   y = y_t, yz = yz_t,
                   Za = za_t, Zb = zb_t, Zp = zp_t, Zq = zq_t,
                   par_prior = c(prior_a, prior_b),
                   par_inits = par_init,
                   traj_init = deviate_states_init,
                   filtering = pgas_run)
} else {
  out_gibbs <- pgas(N = num_particles, MM = num_mcmc, KK = KK, TT = TT,
                    y = y_t, yz = yz_t,
                    Za = za_t, Zb = zb_t, Zp = zp_t, Zq = zq_t,
                    par_prior = c(prior_a, prior_b),
                    par_inits = par_init,
                    traj_init = deviate_states_init,
                    filtering = pgas_run)
}
source("./analysis/2018-11-30/99_analyse_convergence.R")
