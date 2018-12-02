how_long <- function(current, total, len = 5) {
  print_iter <- total/len
  if ((current %% print_iter) == 0) {
    cat(sprintf("###############################################################
              \n"))
    cat(sprintf(" Iteration: %d of : %d completed.\n",
                current, total))
    cat(sprintf(" %.2f%% completed.\n",
                current*100/total))
  }
}
