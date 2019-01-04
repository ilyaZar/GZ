analyse_mcmc_convergence <- function(mcmc_sims, burn, states,
                                     true_vals, par_names, start_vals,
                                     plot_view = FALSE,
                                     plot_ggp2 = FALSE,
                                     plot_save = FALSE,
                                     plot_path = NULL,
                                     plot_name = NULL,
                                     table_view = FALSE,
                                     table_save = FALSE,
                                     table_path = NULL,
                                     table_name = NULL,
                                     ur_view = FALSE,
                                     ur_save = FALSE,
                                     ur_path = NULL,
                                     ur_name = NULL) {
  num_par         <- dim(mcmc_sims)[1]
  num_mcmc        <- dim(mcmc_sims)[2]
  posterior_means <- rowMeans(mcmc_sims[, burn:num_mcmc])
  #
  #
  #
  #
  #
  if (plot_view) {
    if (plot_ggp2) {
      mcmc_sims_df        <- data.frame(cbind(1:num_mcmc, t(mcmc_sims)))
      names(mcmc_sims_df) <- c("num_mcmc", par_names)
      for (i in 1:num_par) {
        par_to_plot <- parse(text = par_names[i])
        hist_plot <- ggplot(data = subset(mcmc_sims_df, num_mcmc >= burn)) +
          geom_density(mapping = aes_string(x = par_names[i])) +
          geom_histogram(aes(x = eval(par_to_plot),
                             y = ..density..),
                         binwidth = 0.025,
                         alpha = 0.5) +
          geom_vline(xintercept = posterior_means[i], colour = "red") +
          geom_vline(xintercept = true_vals[i], colour = "green")

        acfs    <- acf(mcmc_sims_df[par_names[i]], plot = FALSE)
        acfs_df <- with(acfs, data.frame(lag, acf))

        trace_plot_full <- ggplot(data = mcmc_sims_df,
                                  mapping = aes(x = num_mcmc,
                                                y = eval(par_to_plot))) +
          geom_line() +
          geom_hline(yintercept = posterior_means[i], colour = "red") +
          geom_hline(yintercept = true_vals[i], colour = "green")

        trace_plot_burn <- ggplot(data = subset(mcmc_sims_df,
                                                num_mcmc >= burn),
                                  mapping = aes(x = num_mcmc,
                                                y = eval(par_to_plot))) +
          geom_line() +
          geom_hline(yintercept = posterior_means[i], colour = "red") +
          geom_hline(yintercept = true_vals[i], colour = "green")

        acf_plot <- ggplot(data = acfs_df,
                           mapping = aes(x = lag, y = acf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_segment(mapping = aes(xend = lag, yend = 0))

        if (plot_save) {
          current_plot_name <- file.path(plot_path,
                                         paste(plot_name,"_",
                                               par_names[i],
                                               ".eps",
                                               sep = ""))
          print(paste("Saved plots in: ", current_plot_name))
          # setEPS()
          # postscript(current_plot_name)
          # pdf(file = current_plot_name, width = 7, height = 7)
        }
        grid.arrange(hist_plot,
                     trace_plot_full,
                     acf_plot,
                     trace_plot_burn,
                     nrow = 2)
        if (plot_save) {
          dev.off()
        }
      }
    } else{
      for (i in 1:num_par) {
        if (plot_save) {
          current_plot_name <- file.path(plot_path,
                                         paste(plot_name, "_",
                                               par_names[i],
                                               ".eps",
                                               sep = ""))
          print(paste("Saved plots in: ", current_plot_name))
          # postscript(current_plot_name,
          #            horizontal = FALSE,
          #            onefile = FALSE,
          #            paper = "special")
          setEPS()
          postscript(current_plot_name)
          # pdf(file = current_plot_name, width = 7, height = 7)
        }
        par(mfrow = c(2, 2))
        hist(mcmc_sims[i, burn:num_mcmc],
             xlab = par_names[i],
             main = "posterior density")
        abline(v = true_vals[i], col = "green")
        abline(v = posterior_means[i], col = "red")

        plot(mcmc_sims[i, burn:num_mcmc], type = "l",
             xlab = "mcmc iteration",
             ylab = paste(par_names[i], "value", sep = " "),
             main = paste("trace after burnin", burn, sep = ": "))
        abline(h = true_vals[i], col = "green")
        abline(h = posterior_means[i], col = "red")

        coda::autocorr.plot(mcmc_sims[i, ], auto.layout = FALSE)

        plot(mcmc_sims[i, ], type = "l",
             xlab = "mcmc iteration",
             ylab = paste(par_names[i], "value", sep = " "),
             main = paste("complete trace (no burnin)"))
        abline(h = true_vals[i], col = "green")
        abline(h = posterior_means[i], col = "red")
        if (plot_save) {
          dev.off()
        }
      }
    }
  }
  #
  #
  #
  #
  # #
  summary_results <- data.frame(true_value = numeric(num_par),
                    start_value = numeric(num_par),
                    mean = numeric(num_par),
                    sd = numeric(num_par),
                    KI_lower = numeric(num_par),
                    KI_upper = numeric(num_par),
                    containd = logical(num_par))
  row.names(summary_results) <- par_names
  for (i in 1:num_par) {
    summary_results[i, 1] <- true_vals[i]
    summary_results[i, 2] <- start_vals[i]
    summary_results[i, 3] <- posterior_means[i]
    summary_results[i, 4] <- sd(mcmc_sims[i, burn:num_mcmc])
    KI <- quantile(mcmc_sims[i, burn:num_mcmc],
                   probs = c(0.05, 0.95),
                   names = FALSE)
    summary_results[i, 5] <- KI[1]
    summary_results[i, 6] <- KI[2]
    summary_results[i, 7] <- (KI[1] <= true_vals[i] & true_vals[i] <= KI[2])
  }
  if (table_view) {
    View(summary_results, title = paste(table_name, "_summary_results"))
  }
  if (table_save) {
    write_csv(summary_results, path = file.path(table_path,
                                                table_name,
                                                "_summary_results"))
  }
  #
  #
  #
  #
  #
  if (ur_view) {
    par(mfrow = c(1, 1))
    analyse_states_ur(trajectories = res$xtraj)
  }
  if (ur_save) {
    # SAVE PLOTS
  }
}
analyse_states_ur <- function(trajectories) {
  num_trajs  <- length(trajectories)
  num_draws  <- dim(trajectories[[1]])[1]
  num_states <- dim(trajectories[[1]])[2]
  urs <- matrix(0, ncol = num_trajs, nrow = num_states)
  for (i in 1:num_trajs) {
    num_unique_states <- apply(trajectories[[i]], MARGIN = 2, unique)
    num_unique_states <- unlist(lapply(num_unique_states, length))
    urs[, i] <- num_unique_states/num_draws
  }
  # .colMeans(m = num_states, n = num_trajs)
  matplot(urs , type = "l")
}
verify_test <- function(make_correct_test = FALSE,
                        path_test_new,
                        path_test_sol) {
  # this function compares two tests, one defined as the correct solution passed
  # via path_test_sol and the other as the current test to compare with passed
  # via path_test_new
  if (make_correct_test) {
    # analyse_mcmc_convergence with table_view = table_save = TRUE and a
    # table_path set to save an output table of a test as a new
    # correct test solution
    analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                             true_vals  = unlist(par_true[1:2]),
                             start_vals = unlist(par_init[1:2]),
                             par_names  = par_names,
                             states = res$xtraj,
                             burn = burnin,
                             table_view = TRUE,
                             table_save = TRUE,
                             table_path = path_test_sol)
  }
  correct_sol <- read_csv(file = path_test_sol)
  test_sol    <- read_csv(file = path_test_new)
  out <- identical(correct_sol, test_sol)
  if (out) {
    return(cat("Are current test and correct solution identical?\nResult: ",
               green(out),"\n")
    )
  } else {
    return(cat("Are current test and correct solution identical?\nResult: ",
               red(out),"\n")
    )
  }

}
monitor_states <- function(states_drawn, states_true, freeze = 1.5,
                           current, total, num_prints) {
  # if (num_trajs != dim(states_drawn)[2]) {
  #   error("Different number of trajectories to compare!")
  # }
  print_iter <- total/num_prints
  if ((current %% print_iter) != 0) {
    return()
  } else {
    num_trajs <- dim(states_true)[2]
    names_title <- paste(c("True (black) and filtered (red) states for"),
                         c("xa_t", "xb_t", "xp_t", "xq_t"))
    names_ylab  <- paste(c("xa_t", "xb_t", "xp_t", "xq_t"), "states")
    par(mfrow = c(num_trajs, 1))
    for (i in 1:num_trajs) {
      matplot(cbind(states_true[, i], states_drawn[, i]),
              type = "l",
              main = names_title[i],
              ylab = names_ylab[i])
      Sys.sleep(freeze)
    }
  }
}
