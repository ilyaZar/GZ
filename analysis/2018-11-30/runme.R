################################################################################
############################## PGAS for GZ model ###############################
################################################################################
#
#
#
#
#
rm(list = ls())
testrun <- F

source("./R/00_lib_load.R")
source("./R/00_helper_general.R")
source("./R/00_helper_model_fcts.R")
source("./R/00_helper_simulation_data.R")

source("./R/00_settings_simulation.R")

source("./R/01_cBPF_as.R")
source("./R/02_pgas.R")

if (testrun) {
  set.seed(123)
} else {
  set.seed(123)
}
#
#
#
#
#
# PGAS run ----------------------------------------------------------------
res <- pgas(N = num_particles, M = num_mcmc,
            y = yt, Z = zt,
            par_prior = c(prior_a, prior_b),
            par_inits = list(init_sig_sq_x,
                             init_bet_sq_y,
                             init_phi_x,
                             init_bet_x)
)
#
#
#
#
#
source("99_analyse_results.R")
