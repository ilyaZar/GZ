# monitor_mcmc <- function(states_true, states_drawn) {
# }
# changes to be made s.th. state means are tracked for the MCMC iterations (in
# addition to displaying mere computing time until simulation finished)
monitor_pgas_mcmc <- function(current, total, len) {
  print_iter <- total/len
  if ((current %% print_iter) == 0) {
    cat(sprintf("###############################################################
              \n"))
    cat(sprintf("Iteration %d out of %d:\n",
                current, total))
    cat(sprintf("%.2f%% completed.\n",
                current*100/total))
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
