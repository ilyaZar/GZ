generate_x_z <- function(phi_x, sig_sq_x, bet_x,
                         x_level,
                         x_sd,
                         process_exp,
                         intercept,
                         x_init,
                         T,
                         old_regs = old_regs) {
  if (process_exp) {
    x_level <- log(x_level)
  }
  dim_reg <- length(bet_x)
  x <- rep(0, T)
  # BEGINNING OF REGRESSOR SIMULATION: --------------------------------------
  # if (old_regs) {
  #   z <- matrix(rnorm(T*dim_reg), nrow = T, ncol = dim_reg)
  #   if (intercept) {
  #     z[, 1] <- 1
  #   }
  #   par_level_adjust <- z[, -dim_reg, drop = FALSE] %*% bet_x[-dim_reg]
  #   par_level_adjust <- x_level * (1 - phi_x) - par_level_adjust
  #   par_level_adjust <- par_level_adjust/bet_x[dim_reg]
  #   z[, dim_reg]     <- par_level_adjust
  # } else {}
  if (dim_reg == 1) {
    if (intercept) {
      const_level <- x_level * (1 - phi_x)/bet_x
      z <- matrix(const_level, nrow = T, ncol = dim_reg)
    } else {
      const_mean <- x_level * (1 - phi_x)/bet_x
      z          <- matrix(rnorm(T*dim_reg, mean = const_mean, sd = x_sd),
                           nrow = T,
                           ncol = dim_reg,
                           byrow = TRUE)
    }
  } else if (dim_reg == 2) {
    if (intercept) {
      last_zmean <- (x_level * (1 - phi_x) - bet_x[1])/bet_x[2]
      zmeans     <- c(1, last_zmean)
      z          <- matrix(rnorm(T*dim_reg, mean = zmeans, sd = x_sd),
                           nrow = T,
                           ncol = dim_reg,
                           byrow = TRUE)
      z[, 1] <- 1 # rnorm(T, mean = 1, sd = 0.0001)#
    } else {
      zmeans <- rnorm(dim_reg - 1, mean = 0, sd = 3)
      last_zmean <- x_level * (1 - phi_x) - sum(zmeans * bet_x[-dim_reg])
      last_zmean <- last_zmean/bet_x[dim_reg]
      zmeans     <- c(zmeans, last_zmean)
      z          <- matrix(rnorm(T*dim_reg, mean = zmeans, sd = x_sd),
                           nrow = T,
                           ncol = dim_reg,
                           byrow = TRUE)
    }
  } else {
    if (intercept) {
      zmeans <- rnorm(dim_reg - 2, mean = 0, sd = 3)
      zmeans <- c(1, zmeans)
    } else {
      zmeans <- rnorm(dim_reg - 1, mean = 0, sd = 3)
    }
    last_zmean <- x_level * (1 - phi_x) - sum(zmeans * bet_x[-dim_reg])
    last_zmean <- last_zmean/bet_x[dim_reg]
    zmeans     <- c(zmeans, last_zmean)
    z          <- matrix(rnorm(T*dim_reg, mean = zmeans, sd = x_sd),
                         nrow = T,
                         ncol = dim_reg,
                         byrow = TRUE)
    if (intercept) {
      z[, 1] <- 1 # rnorm(T, mean = 1, sd = 0.1)
    }
  }
  # END OF REGRESSOR SIMULATION: --------------------------------------------
  if (x_init == TRUE) {
    xinit <- x_level
  } else {
    xinit <- 0
  }
  # set.seed(123)
  x[1] <- f(x_tt = xinit, z = z[1, ], phi_x = phi_x, bet_x = bet_x)
  x[1] <- x[1] + sqrt(sig_sq_x)*rnorm(n = 1)

  for (t in 1:T) {
    if (t < T) {
      x[t + 1] <- f(x_tt = x[t], z = z[t + 1, ],
                    phi_x = phi_x, bet_x = bet_x)
      x[t + 1] <- x[t + 1] + sqrt(sig_sq_x)*rnorm(n = 1)
    }
  }
  if (process_exp) {
    x <- exp(x)
  }
  if (sum(any(x <= 0)) & process_exp == FALSE) {
    stop("state process (xa_t, xb_t, xp_t or xq_t) out of range (not positive)")
  }
  return(list(x, z))
}
