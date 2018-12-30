regs_a[, 1]  <- log(xa_t[1:(T - 1)])
x_lhs        <- log(xa_t[2:T])

sig_sq_new <- true_sig_sq_xa # sig_sq_xa[m]

Omega_xa     <- solve(crossprod(regs_a, regs_a)/sig_sq_xa[m] + prior_VCM_xa)
mu_xa        <- Omega_xa %*% (crossprod(regs_a, x_lhs)/sig_sq_xa[m])

Omega_xa
mu_xa
# Omega_xa1 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new)
# Omega_xa2 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + 1)
# Omega_xa3 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa)
# Omega_xa4 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa/10)
# Omega_xa5 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa/100)
# Omega_xa6 <- solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa/1000)
#
# Omega_xa1 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
# Omega_xa2 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
# Omega_xa3 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
# Omega_xa4 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
# Omega_xa5 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
# Omega_xa6 %*% (crossprod(regs_a, x_lhs)/sig_sq_xa_new)
#
# kappa(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa)
# kappa(crossprod(regs_a, regs_a)/sig_sq_xa_new + 1)
#
# solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa)
# solve(crossprod(regs_a, regs_a)/sig_sq_xa_new + 1)
#
# (crossprod(regs_a, regs_a)/sig_sq_xa_new + prior_VCM_xa) %*% Omega_xa
# (crossprod(regs_a, regs_a)/sig_sq_xa_new + 1) %*% Omega_xa
