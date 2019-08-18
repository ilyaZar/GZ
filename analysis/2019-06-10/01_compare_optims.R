rm(list = ls())
source("./R/00_helper_lib_load.R")
source("./R/helper_pgas/00_helper_model_fcts.R")
source("./R/helper_pgas/00_helper_simulation_data.R")
source("./R/helper_optim/00_helper_optim.R")
source("./R/helper_optim/01_my_optim.R")
source("./R/helper_optim/01_my_optim2.R")
# PGAS run ----------------------------------------------------------------
set.seed(123) # set.seed(3)
source("./analysis/2019-06-10/00_settings_simulation_data.R")
TT <- 10
KK       <- c(10)     # Number of income classes - 1
num_obs  <- c(10e4)  # Number of total individual incomes

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

test_obs <- 3

res1 <- my_optim(y_t[test_obs,],
                 gini.e = ginis[test_obs],
                 pc.inc = y_raw_means[test_obs],
                 se.gmm = FALSE, se.scale = FALSE,
                 N = num_obs#, #rescale = 1,
                 # gini = TRUE
)
res2 <- my_optim2(y_t[test_obs,],
                  gini.e = ginis[test_obs],
                  pc.inc = y_raw_means[test_obs],
                  se.gmm = FALSE, se.scale = FALSE,
                  N = num_obs#, #rescale = 1,
                  # gini = TRUE
)
identical(res1, res2)
