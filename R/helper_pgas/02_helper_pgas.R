# monitor_mcmc <- function(states_true, states_drawn) {
# }
# changes to be made s.th. state means are tracked for the MCMC iterations (in
# addition to displaying mere computing time until simulation finished)
monitor_pgas_time <- function(current, total, len) {
  print_iter <- total/len
  if ((current %% print_iter) == 0) {
    cat(sprintf("##########################################################\n"))
    cat(sprintf("Iteration %d out of %d: %.2f%% completed.\n",
                current, total, current*100/total))
  }
}
monitor_pgas_mcmc <- function(current, total, len,
                              val_true,
                              val_init,
                              current_pars,
                              dim_all) {
  print_iter <- total/len
  if ((current %% print_iter) == 0) {
    # cat(sprintf("Iteration %d out of %d: %.2f%% completed.\n",
    #             current, total, current*100/total))
    val_mean <- .colMeans(current_pars, m = current, n = dim_all)

    string_format <- paste0(rep("%.3f", times = dim_all), collapse = " ")
    string_print1 <- paste0("init values:     ", string_format, "\n")
    string_print2 <- paste0("true values:     ", string_format, "\n")
    string_print3 <- paste0("mean values:     ", string_format, "\n")

    args_print1 <- c(list(fmt = string_print1), val_init)
    args_print2 <- c(list(fmt = string_print2), val_true)
    args_print3 <- c(list(fmt = string_print3), val_mean)

    cat(sprintf("##########################################################\n"))
    cat(do.call(sprintf, args = args_print1))
    cat(do.call(sprintf, args = args_print2))
    cat(do.call(sprintf, args = args_print3))
  }
}
monitor_pgas_states <- function(states_drawn, states_true, freeze = 1.5,
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

monitor_kf_states <- function(states_mfd, states_jsd, states_true, freeze = 1.5,
                                current, total, num_prints) {
  # if (num_trajs != dim(states_mfd)[2]) {
  #   error("Different number of trajectories to compare!")
  # }
  print_iter <- total/num_prints
  if ((current %% print_iter) != 0) {
    return()
  } else {
    num_trajs <- dim(states_true)[2]
    names_title <- paste(c("True (black), mdf (red), jsd (green) states for"),
                         c("xa_t", "xb_t", "xp_t", "xq_t"))
    names_ylab  <- paste(c("xa_t", "xb_t", "xp_t", "xq_t"), "states")
    par(mfrow = c(num_trajs, 1))
    for (i in 1:num_trajs) {
      matplot(cbind(states_true[, i], states_mfd[, i], states_jsd[, i]),
              type = "l",
              main = names_title[i],
              ylab = names_ylab[i])
      Sys.sleep(freeze)
    }
  }
}
monitor_kf_states_KI <- function(states_kf, states_true,
                                 VCM_kf,
                                 freeze = 1.5,
                                 current = 1, total = 1, num_prints = 1) {
  # if (num_trajs != dim(states_mfd)[2]) {
  #   error("Different number of trajectories to compare!")
  # }
  TT <- nrow(states_kf)
  num_trajs <- dim(states_true)[2]
  KI <- rep(list(matrix(0, nrow = 2, ncol = TT)), times = num_trajs)
  for (t in 1:TT) {
    for (i in 1:num_trajs) {
      bound <- 1.96*sqrt(VCM_kf[[t]][i, i])
      KI[[i]][1, t] <- states_kf[t, i] + bound
      KI[[i]][2, t] <- states_kf[t, i] - bound
    }
  }
  print_iter <- total/num_prints
  if ((current %% print_iter) != 0) {
    return()
  } else {
    names_title <- paste(c("True (black), mdf (red), jsd (green) states for"),
                         c("xa_t", "xb_t", "xp_t", "xq_t"))
    names_ylab  <- paste(c("xa_t", "xb_t", "xp_t", "xq_t"), "states")
    par(mfrow = c(1, 1))
    # browser()
    for (i in 1:num_trajs) {
      matplot(cbind(KI[[i]][1, ], KI[[i]][2, ]),
              type = "l",
              main = names_title[i],
              ylab = names_ylab[i])
      polygon(c(TT:1, 1:TT), c(rev(KI[[i]][1, ]), KI[[i]][2, ]), col = 'grey80', border = NA)
      lines(1:TT, KI[[i]][1, ], lty = 'dashed', col = 'green')
      lines(1:TT, KI[[i]][2, ], lty = 'dashed', col = 'green')
      lines(1:TT, states_true[, i])
      lines(1:TT, states_kf[, i], col = 'blue')
      Sys.sleep(freeze)
    }
  }
}
get_kf_jsd_mean <- function(kf_jsd_matrix_all_states) {
  T <- ncol(kf_jsd_matrix_all_states)
  n_sims <- nrow(kf_jsd_matrix_all_states)
  out <- matrix(0, nrow = 4, ncol = T)
  browser()
  seq1 <- seq(from = 1, by = 4, length.out =  n_sims/4)
  seq2 <- seq(from = 2, by = 4, length.out =  n_sims/4)
  seq3 <- seq(from = 3, by = 4, length.out =  n_sims/4)
  seq4 <- seq(from = 4, by = 4, length.out =  n_sims/4)

  out[1, ] <- colMeans(kf_jsd_matrix_all_states[seq1, ])
  out[2, ] <- colMeans(kf_jsd_matrix_all_states[seq2, ])
  out[3, ] <- colMeans(kf_jsd_matrix_all_states[seq3, ])
  out[4, ] <- colMeans(kf_jsd_matrix_all_states[seq4, ])

  return(out)
}
