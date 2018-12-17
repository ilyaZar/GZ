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
if (testrun) {
  set.seed(123)
} else {
  set.seed(123)
}

source("./R/00_lib_load.R")
source("./R/00_helper_general.R")
source("./R/00_helper_model_fcts.R")
source("./R/00_helper_simulation_data.R")

source("./R/00_settings_simulation.R")

source("./R/01_cBPF_as.R")
source("./R/02_pgas.R")
#
#
#
#
#
# PGAS run ----------------------------------------------------------------
res <- pgas(N = num_particles, M = num_mcmc, K = KK, TT = TT,
            y = y_t, yz = yz_t, Za = za_t,
            par_prior = c(prior_a, prior_b),
            par_inits = par_init)
#
#
#
#
#
source("./R/99_analyse_results.R")
