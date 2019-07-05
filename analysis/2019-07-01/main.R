rm(list = ls())
source("./R/00_helper_lib_load.R")
source("./R/helper_data_analysis/00_helper_data_analyze_plots.R")
source("./analysis/2019-05-15/data_cleaning_main.R")
source("./R/helper_optim/00_helper_optim.R")
source("./R/helper_optim/01_my_optim.R")
# LOAD AND CLEAN DATA: ----------------------------------------------------
load("data/tidy/data_deciles_reg_vars.rda")
data_all <- as.data.frame(data_all)
num_obs <- nrow(data_all)
results_prelim <- matrix(0, nrow = num_obs, ncol = 4)
starting_values_prelim <- matrix(0, nrow = num_obs, ncol = 4)
colnames(results_prelim) <- c("a", "b", "p", "q")
colnames(starting_values_prelim) <- c("a", "b", "p", "q")
# for (i in 1:num_obs) {
#   results_prelim[i, ] <- (GB2group::fitgroup.gb2(data_all[i, 3:12],
#                          gini.e = data_all[i, "gini_reported"]/100,
#                          pc.inc = data_all[i, "mean_usd"],
#                          se.gmm = FALSE, se.scale = FALSE,
#                          N = data_all[i, "population"])$gmm.estimation)[1, ]
#   print(paste(round(i/num_obs, digits = 4)*100, "%"))
# }
for (i in 1:num_obs) {
  res <- (my_optim(data_all[i, 3:12],
                   gini.e = data_all[i, "gini_reported"]/100,
                   pc.inc = data_all[i, "mean_usd"],
                   se.gmm = FALSE, se.scale = FALSE,
                   N = data_all[i, "population"]))
  results_prelim[i, ] <- res$gmm.estimation[1, ]
  starting_values_prelim[i, ] <- res$starting_values
  print(paste(round(i/num_obs, digits = 4)*100, "%", data_all[i, 1]))
}
results_prelim_all <- cbind(data_all[, 1:2],
                            results_prelim,
                            y_mean = data_all$mean,
                            y_mean_usd = data_all$mean_usd,
                            gini_reported = data_all$gini_reported/100)
results_starting_values_all <- cbind(data_all[, 1:2],
                                     starting_values_prelim,
                                     y_mean = data_all$mean,
                                     y_mean_usd = data_all$mean_usd,
                                     gini_reported = data_all$gini_reported/100)
# results_prelim_all[["b"]] <- results_prelim_all[["b"]]*1000
# results_starting_values_all[["b"]] <- results_starting_values_all[["b"]]*1000
#
# View(results_prelim_all)
# source("./analysis/2019-07-01/analyse_latin.R")
source("analysis/2019-07-01/01_analyse_JLP13_reg_bench.R")
