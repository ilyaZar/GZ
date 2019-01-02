if (pgas_run) {
  res <- out_pgas
  subfolder <- "pgas"
} else {
  res <- out_gibbs
  subfolder <- "gibbs"
}
par_mcmc  <- rbind(res$sigma_sq_xa, res$phi_xa, res$bet_xa,
                   res$sigma_sq_xb, res$phi_xb, res$bet_xb,
                   res$sigma_sq_xp, res$phi_xp, res$bet_xp)
par_names <- c("sigma_sq_xa", "phi_xa",
               paste("bet_xa", 1:length(true_bet_xa), sep = "_"),
               "sigma_sq_xb", "phi_xb",
               paste("bet_xb", 1:length(true_bet_xb), sep = "_"),
               "sigma_sq_xp", "phi_xp",
               paste("bet_xp", 1:length(true_bet_xp), sep = "_"))
if (test) {
  path_c <- "/home/chief/Dropbox/research/GZ/analysis/2018-11-30/test_correct"
  path_n <- "/home/chief/Dropbox/research/GZ/analysis/2018-11-30/test_new"
  analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                           true_vals  = unlist(par_true[1:3]),
                           start_vals = unlist(par_init[1:3]),
                           par_names  = par_names,
                           states = res$xtraj,
                           burn = burnin,
                           table_view = TRUE,
                           table_save = TRUE,
                           table_path = path_n)
  verify_test(make_correct_test = FALSE,
              path_test_new = path_n,
              path_test_sol = path_c)
} else {
  analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                           true_vals  = unlist(par_true[1:3]),
                           start_vals = unlist(par_init[1:3]),
                           par_names  = par_names,
                           states = res$xtraj,
                           burn = burnin,
                           plot_view = TRUE,
                           plot_save = FALSE,
                           plot_path = file.path(getwd(),
                                                 "analysis",
                                                 "2018-11-30",
                                                 "doc",
                                                 "fig",
                                                 subfolder),
                           plot_name = "pgas",
                           table_view = TRUE)
}
