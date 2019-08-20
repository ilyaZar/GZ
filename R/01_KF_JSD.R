kf_jsd <- function(y, yz = NULL,
                   Za, Zb, Zp, Zq,
                   # Za_init, Zb_init, Zp_init, Zq_init,
                   KK = NULL, N = NULL, TT,
                   sig_sq_xa, phi_xa, bet_xa, xa_true,
                   sig_sq_xb, phi_xb, bet_xb, xb_true,
                   sig_sq_xp, phi_xp, bet_xp, xp_true,
                   sig_sq_xq, phi_xq, bet_xq, xq_true,
                   VCM_y,
                   filtering = TRUE,
                   MFD = FALSE,
                   JSD = TRUE,
                   n_sim_jsd = 1,
                   n_sim = NULL) {
  if (!filtering) {
    # xa <- matrix(rep(log(xa_t), times = 1), nrow = N, ncol = TT, byrow = TRUE)
    # xb <- matrix(rep(log(xb_t), times = 1), nrow = N, ncol = TT, byrow = TRUE)
    # xp <- matrix(rep(log(xp_t), times = 1), nrow = N, ncol = TT, byrow = TRUE)
    # xq <- matrix(rep(log(xq_t), times = 1), nrow = N, ncol = TT, byrow = TRUE)
    xa <- test_traj_a
    xb <- test_traj_b
    xp <- test_traj_p
    xq <- test_traj_q
    # xa <- xa_t
    # xb <- xb_t
    # xp <- xp_t
    # xq <- xq_t
    return(list(xa, xb, xp, xq))
  }
  # DATA CONTAINERS
  # particles for state processes:
  # xa <- matrix(0, nrow = N, ncol = TT)
  # xb <- matrix(0, nrow = N, ncol = TT)
  # xp <- matrix(0, nrow = N, ncol = TT)
  # xq <- matrix(0, nrow = N, ncol = TT)

  dim_x <- nrow(y)
  A   <- as.matrix(diag(c(phi_xa, phi_xb, phi_xp, phi_xq)))
  # B00 <- rbind(Za_init %*% bet_xa,
  #              Zb_init %*% bet_xb,
  #              Zp_init %*% bet_xp,
  #              Zq_init %*% bet_xq)
  B  <- t(cbind(Za %*% bet_xa,
                Zb %*% bet_xb,
                Zp %*% bet_xp,
                Zq %*% bet_xq))
  C <- as.matrix(diag(rep(1, times = 4)))
  Q <- diag(c(sig_sq_xa, sig_sq_xb, sig_sq_xp, sig_sq_xq))
  R <- VCM_y

  xtt <- matrix(0, nrow = dim_x, ncol = TT)
  Ptt <- rep(list(list()), times = TT)

  # Ptt1    <- tcrossprod(A, tcrossprod(A, P00)) + Q
  Ptt1    <- diag(c(true_sig_sq_xa, true_sig_sq_xb, true_sig_sq_xp, true_sig_sq_xq))

  Lt      <- solve(tcrossprod(C, tcrossprod(C, Ptt1)) + R)
  Kt      <- Ptt1 %*% t(C) %*% Lt

  Ptt[[1]]  <- Ptt1 - Kt %*% Lt %*% t(Kt)
  if (!matrixcalc::is.positive.definite(Ptt[[1]])) {
    stop(paste0("matrix is no longer p.d. at iteration number: ", 1))
  }
  # xtt[, 1]  <- A %*% x00 + B00 + Kt %*% (y[, 1] - C %*% (A %*% x00 + B00))
  xtt[, 1]  <- c(xa_true, xb_true, xp_true, xq_true)
  for (t in 2:TT) {
    Ptt1     <- tcrossprod(A, tcrossprod(A, Ptt[[t - 1]])) + Q
    Lt       <- solve(tcrossprod(C, tcrossprod(C, Ptt1)) + R)
    Kt       <- Ptt1 %*% t(C) # %*% Lt

    Ptt[[t]] <- Ptt1 - Kt %*% Lt %*% t(Kt)
    if (!matrixcalc::is.positive.definite(Ptt[[t]])) {
      stop(paste0("matrix is no longer p.d. at iteration number: ", t))
    }
    xtt1 <-  A %*% xtt[, t - 1] + B[, t - 1]
    xtt[, t] <- xtt1 + Kt %*% Lt %*% (y[, t] - C %*% (xtt1))  # %*% Lt
  }
  if (MFD) {
    kf_MFD_res <- rep(list(list()), times = n_sim)
    for (n in 1:n_sim) {
      kf_MFD <- matrix(0, ncol = TT, nrow = dim_x)
      for (t in 1:TT) {
        kf_MFD[, t] <- mvrnorm(1, mu = xtt[, t], Sigma = Ptt[[t]])
      }
      kf_MFD_res[[n]] <- kf_MFD
      browser()
      monitor_kf_states_KI(t(kf_MFD), cbind(xa_t, xb_t, xp_t, xq_t),
                           Ptt,
                           freeze = 1.5)
    }
  }
  if (JSD) {
    kf_JSD_all <- matrix(0, nrow = dim_x*n_sim_jsd, ncol = TT)
    for (n in 1:n_sim_jsd) {
      Jtt <- rep(list(list()), times = TT)
      Ptt_jsd <- rep(list(list()), times = TT)
      xtt_jsd <- matrix(0, nrow = dim_x, ncol = TT)
      kf_JSD  <- matrix(0, nrow = dim_x, ncol = TT)
      # browser()
      Ptt_jsd[[TT]] <- Ptt[[TT]]
      xtt_jsd[, TT] <- xtt[, TT]
      kf_JSD[, TT]  <- mvrnorm(1, mu = xtt_jsd[, TT], Sigma = Ptt_jsd[[TT]])
      # kf_JSD[, TT]  <- c(xa_t[TT], xb_t[TT], xp_t[TT], xq_t[TT])
      for (t in (TT - 1):1) {
        Jtt[[t]] <- Ptt[[t]] %*% t(A) %*% solve(tcrossprod(A, tcrossprod(A, Ptt[[t]])) + Q)
        Ptt_jsd[[t]] <-  Ptt[[t]] - Jtt[[t]] %*% A %*% Ptt[[t]]
        xtt_jsd[, t]  <- xtt[, t] + Jtt[[t]]  %*% (kf_JSD[, t + 1] - B[, t] - A %*% xtt[, t])

        kf_JSD[, t] <- mvrnorm(1, mu = xtt_jsd[, t], Sigma = Ptt_jsd[[t]])
      }
      # browser()
      # monitor_kf_states_KI(t(xtt_jsd), cbind(xa_t, xb_t, xp_t, xq_t),
      #                      Ptt_jsd,
      #                      freeze = 1.5)
      # print(n)
      # row_ID <- (dim_x*(n - 1) + 1):(dim_x*n)
      # kf_JSD_all[row_ID, ] <-  kf_JSD
      kf_JSD_all <-  kf_JSD
    }
    # browser()
    # kf_JSD_means <- get_kf_jsd_mean(kf_JSD_all)
    # monitor_kf_states_KI(t(kf_JSD_means), cbind(xa_t, xb_t, xp_t, xq_t),
    #                      Ptt_jsd,
    #                      freeze = 1.5)
  }
  return(list(xa = kf_JSD_all[1, ],
              xb = kf_JSD_all[2, ],
              xp = kf_JSD_all[3, ],
              xq = kf_JSD_all[4, ]))
}
