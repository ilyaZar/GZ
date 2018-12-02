if (testrun) print(res)

par(mfrow = c(2,2))

plot(res$sigma_sq_x, type = "l")
plot(res$beta_sq_y, type = "l")
plot(res$sigma_sq_x[burnin:num_mcmc], type = "l")
plot(res$beta_sq_y[burnin:num_mcmc], type = "l")

plot(res$phi_x, type = "l")
for (i in 1:length(res$bet_x[, 1])) {
  plot(res$bet_x[i, ], type = "l")
}
plot(res$phi_x[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_x[, 1])) {
  plot(res$bet_x[i, burnin:num_mcmc], type = "l")
}

par(mfrow = c(1,2))
hist(res$sigma_sq_x[burnin:num_mcmc], type = "l")
hist(res$beta_sq_y[burnin:num_mcmc], type = "l")

hist(res$phi_x[burnin:num_mcmc], type = "l")
for (i in 1:length(res$bet_x[, 1])) {
  hist(res$bet_x[i, burnin:num_mcmc], type = "l")
}

print(mean(res$sigma_sq_x[burnin:num_mcmc]))
print(sd(res$sigma_sq_x[burnin:num_mcmc]))

print(mean(res$beta_sq_y[burnin:num_mcmc]))
print(sd(res$beta_sq_y[burnin:num_mcmc]))

print(mean(res$phi_x[burnin:num_mcmc]))
print(sd(res$phi_x[burnin:num_mcmc]))

print(rowMeans(res$bet_x[, burnin:num_mcmc]))
print(apply(res$bet_x[, burnin:num_mcmc], 1, sd))
