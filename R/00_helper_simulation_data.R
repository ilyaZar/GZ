generate_data <- function(T, sig_sq_x, bet_sq_y, phi_x, bet_x) {
  x <- rep(0, T)
  y <- rep(0, T)
  
  z <- matrix(rnorm(T*length(bet_x)), nrow = T, ncol = length(bet_x))
  z[, 1] <- 1
  
  xinit <- 0
  x[1] <- f(xtt = xinit, z = z[1, ], t = 1, phi_x = phi_x, bet_x = bet_x)
  x[1] <- x[1] + sqrt(sig_sq_x)*rnorm(n = 1)
  # x[1] <- 0
  for (t in 1:T) {
    if (t < T) {
      x[t + 1] <- f(xtt = x[t], z = z[t + 1, ], t = t,
                    phi_x = phi_x, bet_x = bet_x) 
      x[t + 1] <- x[t + 1] + sqrt(sig_sq_x)*rnorm(n = 1)
    }
    y[t] <- sqrt(bet_sq_y*exp(x[t]))*rnorm(1)
    # y[t] <- g(x[t]) + sqrt(bet_sq_y)*rnorm(1)
  }
  return(list(x, y, z))
}
