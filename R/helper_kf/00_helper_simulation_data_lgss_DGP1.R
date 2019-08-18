generate_data_DGP_1 <- function(T, K, num_incs,
                                par_true,
                                x_levels,
                                seq_logs,
                                seq_cept,
                                old_regs = FALSE,
                                plot_states) {
  xa <- rep(0, T)
  xb <- rep(0, T)
  xp <- rep(0, T)
  xq <- rep(0, T)

  sig_sq_xa <- par_true[[1]][[1]]
  phi_xa    <- par_true[[1]][[2]]
  bet_xa    <- par_true[[1]][[3]]
  sig_sq_xb <- par_true[[2]][[1]]
  phi_xb    <- par_true[[2]][[2]]
  bet_xb    <- par_true[[2]][[3]]
  sig_sq_xp <- par_true[[3]][[1]]
  phi_xp    <- par_true[[3]][[2]]
  bet_xp    <- par_true[[3]][[3]]
  sig_sq_xq <- par_true[[4]][[1]]
  phi_xq    <- par_true[[4]][[2]]
  bet_xq    <- par_true[[4]][[3]]
  # Generate regressors -----------------------------------------------------
  res_a <- generate_x_z(phi_x = phi_xa, sig_sq_x = sig_sq_xa, bet_x = bet_xa,
                        x_level     = x_levels[1],
                        x_sd = 0.0125,
                        process_exp = seq_logs[1],
                        intercept   = seq_cept[1],
                        x_init = TRUE,
                        T = T,
                        old_regs = old_regs)
  xa    <- res_a[[1]]
  za    <- res_a[[2]]
  res_b <- generate_x_z(phi_x = phi_xb, sig_sq_x = sig_sq_xb, bet_x = bet_xb,
                        x_level     = x_levels[2],
                        x_sd = 0.1,
                        process_exp = seq_logs[2],
                        intercept   = seq_cept[2],
                        x_init = TRUE,
                        T = T,
                        old_regs = old_regs)
  xb    <- res_b[[1]]
  zb    <- res_b[[2]]
  res_p <- generate_x_z(phi_x = phi_xp, sig_sq_x = sig_sq_xp, bet_x = bet_xp,
                        x_level = x_levels[3],
                        x_sd = 0.025,
                        process_exp = seq_logs[3],
                        intercept   = seq_cept[3],
                        x_init = TRUE,
                        T = T,
                        old_regs = old_regs)
  xp    <- res_p[[1]]
  zp    <- res_p[[2]]
  res_q <- generate_x_z(phi_x = phi_xq, sig_sq_x = sig_sq_xq, bet_x = bet_xq,
                        x_level     = x_levels[4],
                        x_sd = 0.1,
                        process_exp = seq_logs[4],
                        intercept   = seq_cept[4],
                        x_init = TRUE,
                        T = T,
                        old_regs = old_regs)
  xq <- res_q[[1]]
  zq <- res_q[[2]]
  # Generate individual incomes, group boundaries and grouped income --------
  yraw <- matrix(rgb2(n = num_incs*T,
                      shape1 = xa,
                      scale  = xb,
                      shape2 = xp,
                      shape3 = xq),
                 nrow = T, ncol = num_incs)
  seq_prob <- rep(seq(from = 0, to = 1 - (1/K), length.out = K), each = T)
  yz <- matrix(qgb2(prob = seq_prob,
                    shape1 = xa,
                    scale  = xb,
                    shape2 = xp,
                    shape3 = xq),
               nrow = T, ncol = K)
  yz   <- cbind(yz, rep(Inf, times = T))
  yt  <- matrix(0, nrow = T, ncol = K)
  for (t in 1:T) {
    ncut <- cut(yraw[t, ], breaks = yz[t, ])
    yt[t, ] <- as.vector(table(ncut))
  }
  yz <- yz[, -(K + 1)]
  # Optional state plotting -------------------------------------------------
  if (plot_states) {
    names_title <- paste("True states for ",
                         "xa_t (black),", " xb_t (red),",
                         " xp_t (green),", " and", " xq_t (blue)")
    names_ylab  <- paste(" xa_t,", " xb_t,", " xp_t,",
                         " and", " xq_t", " states")

    par(mfrow = c(1,1))
    matplot(cbind(xa, xb, xp, xq),
            type = "l",
            main = names_title,
            ylab = names_ylab
    )
  }
  return(list(yt, yz, list(xa, xb, xp, xq), list(za, zb, zp, zq), yraw))
}
