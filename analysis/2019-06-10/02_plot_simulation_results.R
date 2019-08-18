plots_classes <- rep(list(list()), times = length(KK_vec))
plots_sample_size <- rep(list(list()), times = num_sample_size)
for (k in 1:length(KK_vec)) {
  KK <- KK_vec[k]
  plots_classes[[k]] <- plots_sample_size
  for (n in 1:num_sample_size) {
    results_par_gmm <- matrix(0, nrow = TT, ncol = 4)
    pars <- c("a", "b", "p", "q")
    colnames(results_par_gmm) <- pars
    for (t in 1:TT) {
      results_par_gmm[t, ] <- results_gmm_estimation[[k]][[n]][[t]]$gmm.estimation[1, ]
    }
    results_par_gmm[, 2] <- results_par_gmm[, 2]*1000
    esimation_means <- colMeans(results_par_gmm, na.rm = TRUE)
    plot_list <- rep(list(list()), times = 4)

    for (j in 1:4) {
    plot_list[[j]] <- ggplot(data = as.tibble(results_par_gmm),
                             mapping = aes_string(x = pars[j])) +
      geom_density() +
      geom_histogram(mapping = aes(y = ..density..),
                     bins = 20,
                     alpha = 0.5) +
      geom_vline(xintercept = esimation_means[j], colour = "red") +
      geom_vline(xintercept = par_levels[j], colour = "green")
    } #
    plot_title <- paste0("No. of classes: ", KK,". ",
                        "Sample Size: ", num_obs_vec[n], ".")
    plots_classes[[k]][[n]] <- grid.arrange(grobs = plot_list,
                                           top = plot_title)
    plot_name <- paste0("_K=", KK, "_N=", num_obs_vec[n], ".pdf")
    ggsave(filename = paste0("results/simulation_studies/gmm-k=5_10-N=10e2_10e3/", plot_name),
           plot = plots_classes[[k]][[n]])
  }
}
