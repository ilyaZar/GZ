data_gen_lgssm <- function(T, dim_x, dim_y,  A, B, C, D, Q, R, initX) {
  x0 <- initX
  
  if (dim_x == 1) {
  xt <- numeric(T)
  yt <- numeric(T)
  zt <- rnorm(1, 0, 1)
  ut <- rnorm(1, 0, 1)
  
  xt[1] <- A*x0 + B*zt[1] + rnorm(1, 0, sd = Q^2)
  for (t in 2:T) {
    xt[t] <- A*xt[t - 1] + B*zt[t] + rnorm(1, 0, sd = Q^2)
  }
  yt <- C*xt + D*ut + rnorm(T, 0, sd = R)
  return(list(xt, yt))  
  } else if (dim_x >= 2) {
  xt <- matrix(0, nrow = dim_x, ncol = T)
  yt <- matrix(0, nrow = dim_y, ncol = T)
  zt <- matrix(rnorm(dim_x*T, 0, 1), nrow = dim_x, ncol = T)
  ut <- matrix(rnorm(dim_y*T, 0, 1), nrow = dim_y, ncol = T)
  z0 <- rnorm(dim_x, 0, 1)
  
  xt[, 1] <- A*x0 + B*z0 + mvrnorm(n = 1, mu = rep(0, times = dim_x), Sigma = Q)
  for (t in 2:T) {
    xt[, t] <- A*xt[, t - 1] + B*zt[, t] + mvrnorm(n = 1, mu = rep(0, times = dim_x), Sigma = Q)
    yt[, t] <- C*xt[, t] + D*ut[, t] +  mvrnorm(n = 1, mu = rep(0, times = dim_y), Sigma = R)
  }
  return(list(states = xt, measurements = yt, x_reg = zt, y_reg = ut, z_init = z0, x_init = x0))
  }
}
kf_lgssm <- function(yt, zt, ut, A, B, C, D, Q, R,
                     P00, x00, z00,
                     n_sim = 1,
                     n_sim_jsd = 1,
                     MFD = TRUE,
                     JSD = TRUE) {
  T     <- ncol(yt)
  dim_x <- nrow(yt)
  A <- as.matrix(diag(A))
  B <- as.matrix(diag(B))
  C <- as.matrix(diag(C))
  D <- as.matrix(diag(D))
  
  xtt <- matrix(0, nrow = dim_x, ncol = T)
  Ptt <- rep(list(list()), times = T)

  Ptt1    <- tcrossprod(A, tcrossprod(A, P00)) + Q
  Lt      <- solve(tcrossprod(C, tcrossprod(C, Ptt1)) + R)
  Kt      <- Ptt1 %*% t(C) %*% Lt

  Ptt[[1]]  <- Ptt1 - Kt %*% Lt %*% t(Kt)
  if (!matrixcalc::is.positive.definite(Ptt[[1]])) {
    stop(paste0("matrix is no longer p.d. at iteration number: ", 1))
  }
  xtt[, 1]  <- A %*% x00 + B %*% z00 + Kt %*% (yt[, 1] - C %*% (A %*% x00 + B %*% z00) - D %*% ut[, 1])
  for (t in 2:T) {
    Ptt1     <- tcrossprod(A, tcrossprod(A, Ptt[[t - 1]])) + Q
    Lt       <- solve(tcrossprod(C, tcrossprod(C, Ptt1)) + R)
    Kt       <- Ptt1 %*% t(C) %*% Lt

    Ptt[[t]] <- Ptt1 - Kt %*% Lt %*% t(Kt)
    if (!matrixcalc::is.positive.definite(Ptt[[t]])) {
      stop(paste0("matrix is no longer p.d. at iteration number: ", t))
    }
    xtt[, t] <- A %*% xtt[, t - 1] + B %*% zt[, t - 1] + Kt %*% (yt[, t] - C %*% (A %*% xtt[, t - 1] + B %*% zt[, t - 1]) - D %*% ut[, t])
  }
  if (MFD) {
    kf_MFD_res <- rep(list(list()), times = n_sim)
    for (n in 1:n_sim) {
      kf_MFD <- matrix(0, ncol = T, nrow = dim_x)
      for (t in 1:T) {
        kf_MFD[, t] <- mvrnorm(1, mu = xtt[, t], Sigma = Ptt[[t]])    
      }
      kf_MFD_res[[n]] <- kf_MFD
    }
  }
  if (JSD) {
    kf_JSD_all <- matrix(0, nrow = dim_x*n_sim_jsd, ncol = T)
    for (n in 1:n_sim_jsd) {
      Jtt <- rep(list(list()), times = T)
      xtt_jsd <- matrix(0, nrow = dim_x, ncol = T)
      kf_JSD  <- matrix(0, nrow = dim_x, ncol = T)

      kf_JSD[, T] <- mvrnorm(1, mu = xtt[, T], Sigma = Ptt[[T]])    

      for (t in (T - 1):1) {
        Jtt[[t]] <- Ptt[[t]] %*% t(A) %*% solve(tcrossprod(A, tcrossprod(A, Ptt[[t]])) + Q)
        xtt_jsd[, t]  <- xtt[, t] + Jtt[[t]]  %*% (kf_JSD[, t + 1] - B %*% zt[, t] - A %*% xtt[, t])
        kf_JSD[, t] <- mvrnorm(1, mu = xtt_jsd[, t], Sigma = Jtt[[t]])    
      }
      # browser()
      print(n)
      row_ID <- (dim_x*(n - 1) + 1):(dim_x*n)
      kf_JSD_all[row_ID, ] <-  kf_JSD
    }
  }
  return(list(kf_marginal_filtering_density = kf_MFD_res, kf_VCM_mfd = Ptt,
              kf_joint_smoothing_density = kf_JSD_all, kf_VCM_jsd = Jtt))
}









# kflg <- function(yt, A, C, Q, R, P00, x00, n_sim = 1) {
#   T    <- length(yt)
#   xtt  <- numeric(T)
#   Ptt  <- numeric(T)
#   # Ptt1    <- A %*% P00 %*% A  + Q
#   Ptt1    <- tcrossprod(A, tcrossprod(A, P00))
#   Lt <- 
#   Kt      <- Ptt1*C*(((C^2)*Ptt1 + R)^(-1))
#   Ptt[1]  <- Ptt1 - Kt*C*Ptt1                        
#   xtt[1]  <- A*x00 + Kt*(yt[1] - C*A*x00)
#   for (t in 2:T) {
#     Ptt1     <- (A^2)*Ptt[t - 1] + Q
#     Kt     <- Ptt1*C*(((C^2)*Pttm + R)^(-1))
#     Ptt[t] <- Ptt1 - Kt*C*Ptt1
#     xtt[t] <- A*xtt[t - 1] + Kt*(yt[t] - C*A*xtt[t - 1])
#   }
#   # KFapprox <- xtt
#   res_sim <- matrix(0, ncol = T, nrow = n_sim)
#   for (n in 1:n_sim) {
#     res_sim[n, ] <- rnorm(T, mean = xtt, sd = sqrt(Ptt))  
#   }
#   if (n_sim == 1) {
#     res_sim <- as.vector(res_sim)
#   }
#   return(list(KFx = res_sim, KFvar = Ptt))
# }
