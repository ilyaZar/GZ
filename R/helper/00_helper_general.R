how_long <- function(current, total, len = 5) {
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
monitor_states <- function(states_drawn, states_true, freeze = 2) {
  num_trajs <- dim(states_true)[2]
  if (num_trajs != dim(states_drawn)[2]) {
    error("different number of trajectories to compare!")
  }
  for (i in 1:num_trajs) {
    matplot(cbind(states_true[, i], states_drawn[, i]), type = "l")
    Sys.sleep(freeze)
  }
}
monitor_mcmc <- function(states_true, states_drawn) {
}
