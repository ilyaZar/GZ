# monitor_mcmc <- function(states_true, states_drawn) {
# }
# rename how_long into monitor_mcmc() if changes are made s.th. state means are
# tracked for the MCMC iterations (in addition to displaying mere computing time
# until simulation finished)
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
