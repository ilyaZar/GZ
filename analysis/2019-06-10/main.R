rm(list = ls())
library(GB2group)
library(GB2)
library(minpack.lm)
source("./R/00_helper_lib_load.R")
source("./R/helper_pgas/00_helper_model_fcts.R")
source("./R/helper_pgas/00_helper_simulation_data.R")
source("./R/helper_optim/00_helper_optim.R")
source("./R/helper_optim/01_my_optim.R")
# PGAS run ----------------------------------------------------------------
set.seed(123) # set.seed(3) #
source("./analysis/2019-06-10/00_settings_simulation_data.R")
ginis <- numeric(TT)
for (t in 1:TT) {
  ginis[t] <- gini.gb2(xa_t[t], xp_t[t], xq_t[t])
}
y_raw_means <- rowMeans(dataSim[[5]])

num_obs <- 90
fitgroup.gb2(y_t[num_obs,],
             gini.e = ginis[num_obs],
             pc.inc = y_raw_means[num_obs],
             se.gmm = FALSE, se.scale = FALSE,
             N = num_obs, #rescale = 1,
             gini = TRUE
)
my_optim(y_t[num_obs,],
         gini.e = ginis[num_obs],
         pc.inc = y_raw_means[num_obs],
         se.gmm = FALSE, se.scale = FALSE,
         N = num_obs, #rescale = 1,
         gini = TRUE
)
xa_t[num_obs]
xb_t[num_obs]
xp_t[num_obs]
xq_t[num_obs]

