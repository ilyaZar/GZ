rm(list = ls())
source("./R/helper_data_analyze/00_helper_data_analyze_plots.R")
source("./analysis/2019-05-15/data_cleaning_main.R")
# LOAD AND CLEAN DATA: ----------------------------------------------------
load("data/tidy/data_deciles_reg_vars.rda")
data_all <- as.data.frame(data_all)
num_obs <- nrow(data_all)
results_prelim <- matrix(0, nrow = num_obs, ncol = 4)
colnames(results_prelim) <- c("a", "b", "p", "q")
for (i in 1:num_obs) {
  results_prelim[i, ] <- (GB2group::fitgroup.gb2(data_all[i, 3:12],
                         gini.e = data_all[i, "gini_reported"]/100,
                         pc.inc = data_all[i, "mean_usd"],
                         se.gmm = FALSE, se.scale = FALSE,
                         N = data_all[i, "population"])$gmm.estimation)[1, ]
  print(paste(round(i/num_obs, digits = 4)*100, "%"))
}
results_prelim_all <- cbind(data_all[, 1:2],
                            results_prelim,
                            y_mean = data_all$mean,
                            y_mean_usd = data_all$mean_usd,
                            gini_reported = data_all$gini_reported/100)
View(results_prelim_all)
# source("./analysis/2019-07-01/analyse_latin.R")
source("analysis/2019-07-01/01_analyse_JLP13_reg_bench.R")


