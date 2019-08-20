# source("./R/00_lib_load.R")
# source("./R/01_KF_helper.R")
# set.seed(123)
# TT <- 100
# a <- c(0.5, 0.7)
# b <- c(0.8, 0.7)
# c <- c(0.8, 0.9)
# d <- c(0.8, 0.6)
# dim_model <- 2
# dim_X     <- dim_model
# dim_Y     <- dim_model
# q <- diag(sqrt(1)*rep(1, times = dim_X))
# r <- diag(sqrt(1)*rep(1, times = dim_Y))
# x0 <- rep(0, times = dim_X) #rnorm(1, 0, 1)
#
# data_sim <- data_gen_lgssm(T = TT, dim_x = dim_X, dim_y = dim_Y,
#                            A = a, B = b,
#                            C = c, D = d,
#                            Q = q, R = r,
#                            initX = 1:dim_X)
#
# xt     <- data_sim[["states"]]
# yt     <- data_sim[["measurements"]]
# z_init <- data_sim[["z_init"]]
# x_init <- data_sim[["x_init"]]
# y_reg  <- data_sim[["y_reg"]]
# x_reg  <- data_sim[["x_reg"]]
#
# par(mfrow = c(1, 2))
# for (i in 1:dim_model) {
#   plot(xt[i, ], type = "l", main = paste("Simulated LGSSM: component ", i),
#        ylab = "measurements and states", xlab = "t")
#   lines(yt[i, ], type = "l", col = "green")
# }
# num_jsd <- 5000
# kf_lgssm_res <- kf_lgssm(yt = yt, zt = x_reg, ut = y_reg,
#                          A = a, B = b, C = c, D = d, Q = q, R = r,
#                          P00 = diag(c(1, 1)), x00 = x_init, z00 = z_init,
#                          n_sim = 1,
#                          n_sim_jsd = num_jsd,
#                          MFD = TRUE,
#                          JSD = TRUE)
# kf_mfd <- kf_lgssm_res[[1]][[1]]
# kf_VCM_mfd <- kf_lgssm_res[[2]]
# kf_jsd <- kf_lgssm_res[[3]]
# kf_VCM_jsd <- kf_lgssm_res[[4]]
#
# num_state <- 1
# ID_state  <- seq(from = num_state, by = dim_X, length.out = num_jsd)
# state_current_analysis <- kf_jsd[ID_state, ]
# mean_kf_smooth_1 <- colMeans(state_current_analysis)
# View(rbind(xt[num_state, ], matrix(mean_kf_smooth_1, byrow = TRUE, nrow = 1)))
#
# par(mfrow = c(1, 1))
# for (t in 1:TT) {
#   hist(state_current_analysis[, t], main = paste0("state No. ", t))
#   abline(v = mean(state_current_analysis[, t]), col = "red")
#   abline(v = xt[num_state, t], col = "green")
#   Sys.sleep(5)
# }
#
#
#
#
#
# par(mfrow = c(1, 2))
# for (i in 1:dim_model) {
#   plot(xt[i, ], type = "l", main = paste("Simulated LGSSM: component ", i),
#        ylab = "measurements and states", xlab = "t")
#   # lines(yt[i, ], type = "l", col = "green")
#   lines(kf_mfd[i, ], type = "l", col = "blue")
#   KFapprox_KI <- matrix(0, nrow = 2, ncol = TT)
#   kf_var <- numeric(TT)
#   for (t in 1:TT) {
#     kf_var[t] <- kf_VCM_mfd[[t]][i, i]
#   }
#   KFapprox_KI[1,] <- kf_mfd[i, ] + 1.96*sqrt(kf_var[t])
#   KFapprox_KI[2,] <- kf_mfd[i, ] - 1.96*sqrt(kf_var[t])
#   lines(KFapprox_KI[1, ], col = "red", lty = 2)
#   lines(KFapprox_KI[2, ], col = "red", lty = 2)
# }
# #
# #
# #
# #
# #
# # # Initialization
# # X00 <- xt[1]
# # P00 <- q
# # # Plotting results:
# # plot(xt, type = "l", xlab = "t", ylab = "True states and KF-approx.")
# # KFapprox_all <- kflg(yt = measurement, A = a, C = c, Q = q^2, R = r^2,
# #                      p00 = P00, x00 = X00)
# # KFapprox <- KFapprox_all$KFx
# # KFapprox_KI <- matrix(0, nrow = 2, ncol = TT)
# # KFapprox_KI[1,] <- KFapprox + 1.96*sqrt(KFapprox_all$KFvar)
# # KFapprox_KI[2,] <- KFapprox - 1.96*sqrt(KFapprox_all$KFvar)
# # lines(KFapprox, col = "red")
# # lines(KFapprox_KI[1, ], col = "red", lty = 2)
# # lines(KFapprox_KI[2, ], col = "red", lty = 2)
