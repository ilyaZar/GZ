if (testrun) print(res)

par(mfrow = c(2,2))

plot(res$sigma_sq_xa, type = "l")
plot(res$sigma_sq_xa[burnin:num_mcmc], type = "l")

plot(res$phi_xa, type = "l")
for (i in 1:length(res$bet_xa[, 1])) {
  plot(res$bet_xa[i, ], type = "l")
}
plot(res$phi_xa[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_xa[, 1])) {
  plot(res$bet_xa[i, burnin:num_mcmc], type = "l")
}

par(mfrow = c(1,2))
hist(res$sigma_sq_xa[burnin:num_mcmc], type = "l")

hist(res$phi_xa[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_xa[, 1])) {
  hist(res$bet_xa[i, burnin:num_mcmc], type = "l")
}

print(mean(res$sigma_sq_xa[burnin:num_mcmc]))
print(sd(res$sigma_sq_xa[burnin:num_mcmc]))

print(mean(res$phi_xa[burnin:num_mcmc]))
print(sd(res$phi_xa[burnin:num_mcmc]))

print(rowMeans(res$bet_xa[, burnin:num_mcmc]))
print(apply(res$bet_xa[, burnin:num_mcmc], 1, sd))
#
#
#
#
#
plot(res$sigma_sq_xb, type = "l")
plot(res$sigma_sq_xb[burnin:num_mcmc], type = "l")

plot(res$phi_xb, type = "l")
for (i in 1:length(res$bet_xb[, 1])) {
  plot(res$bet_xb[i, ], type = "l")
}
plot(res$phi_xb[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_xb[, 1])) {
  plot(res$bet_xb[i, burnin:num_mcmc], type = "l")
}

par(mfrow = c(1,2))
hist(res$sigma_sq_xb[burnin:num_mcmc], type = "l")

hist(res$phi_xb[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_xb[, 1])) {
  hist(res$bet_xb[i, burnin:num_mcmc], type = "l")
}

print(mean(res$sigma_sq_xb[burnin:num_mcmc]))
print(sd(res$sigma_sq_xb[burnin:num_mcmc]))

print(mean(res$phi_xb[burnin:num_mcmc]))
print(sd(res$phi_xb[burnin:num_mcmc]))

print(rowMeans(res$bet_xb[, burnin:num_mcmc]))
print(apply(res$bet_xb[, burnin:num_mcmc], 1, sd))

