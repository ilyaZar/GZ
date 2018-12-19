my_ws <- cumsum(seq(from = 0.1, to = 0.5, by = 0.1))
my_ws <- my_ws/sum(my_ws)

m          <- -1/2 * helper_as(M = m2, x = m1)/10000
w_log_as   <- log(my_ws) + m
w_max_as   <- max(w_log_as)
w_tilde_as <- exp(w_log_as - w_max_as)
w_as1       <- w_tilde_as/sum(w_tilde_as)

m          <- exp(-1/2 * helper_as(M = m2, x = m1)/10000)
w_as       <- my_ws*m
w_as2      <- w_as/sum(w_as)

identical(round(w_as1, digits = 8), round(w_as2, digits = 8))
