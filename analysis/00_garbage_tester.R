


sort(yraw[58, ])[1:10]
yraw_sorted[, 58][1:10]

sort(yraw[1, ])[99900:100000]
yraw_sorted[, 1][99900:100000]

identical(sort(yraw[10, ]), yraw_sorted[, 11])



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


d_test <- d[1:3, 1:3]
xp_test <- xp[1:3]
pbeta(q = d_test, shape1 = exp(xp_test), shape2 = xq[1:3])
xq_t




mu_log_norm <- 150
sd_log_norm <- 100

core_transform <- 1 + sd_log_norm^2/mu_log_norm^2
mu_norm <- log(mu_log_norm/(sqrt(core_transform)))
sd_norm <- sqrt(log(core_transform))

mu_norm
sd_norm^2

test_norm <- rnorm(n = 1000, mean = mu_norm, sd = sd_norm)
test_log_norm <- exp(test_norm)
mean(test_log_norm)
sd(test_log_norm)
plot(test_log_norm, type = "l")




N

hist(xa)
abline(v = log(xa_t[1]), col = "red")

hist(xb)
abline(v = log(xb_t[1]), col = "red")

hist(xp)
abline(v = log(xp_t[1]), col = "red")

hist(xq)
abline(v = log(xq_t[1]), col = "red")


NN   <- 1000

xa_true <- rep(xa_t[1], times = NN)
xb_true <- rep(log(xb_t[1]), times = NN)
xp_true <- rep(log(xp_t[1]), times = NN)
xq_true <- rep(log(xq_t[1]), times = NN)

xa_test <- seq(from = 0.26, to = 0.29, length.out = 1000)# xa
# xa_test <- sort(xa)
# xa_test <- xa[xa <= 0.32 & xa >= 0.28]
xa_test <- rnorm(1000, mean = 0.2755, sd = 0.01)
xa_test <- xa
NN <- length(xa_test)

xb_true <- rep(log(xb_t[1]), times = NN)
xp_true <- rep(log(xp_t[1]), times = NN)
xq_true <- rep(log(xq_t[1]), times = NN)

xb_test <- xb_true
xp_test <- xp_true
xq_test <- xq_true

# NN   <- length(xa_test)
zks <- matrix(rep(yz, times = NN), nrow = NN, byrow = TRUE)
d <- zks/exp(xb_test)
d <- d^exp(xa_test)
d <- d/(1 + d)

if (any(is.nan(d))) {d[is.nan(d)] <- 1}

F_gb2 <- pbeta(q = d, shape1 = exp(xp_test), shape2 = exp(xq_test))
F_gb2 <- cbind(F_gb2, rep(1, times = NN))

pi_prob <- F_gb2[, 2:(KK + 1)] - F_gb2[, 1:KK]

if (any(pi_prob < 0)) {pi_prob[pi_prob < 0] <- 0}

pi_prob <- log(pi_prob)

pi_prob <- t(pi_prob)*y
w <- .colSums(pi_prob, m = KK, n = NN)

plot(xa_test, w, type = "l")
abline(v = log(xa_t[1]), col = "red")
# abline(v = mean(xa), col = "blue")

w_log   <- sort(w, decreasing = TRUE)[1:10]# w #
w_max   <- max(w_log)
w_tilde <- exp(w_log - w_max)
w_max   <- max(w_log)
w_tilde <- w_tilde/sum(w_tilde)
hist(w_tilde)





d <- yz/exp(xb_t[1])
d <- d^exp(0.5)
d <- d/(1 + d)

if (any(is.nan(d))) {d[is.nan(d)] <- 1}

F_gb2 <- pbeta(q = d, shape1 = exp(xp_t[1]), shape2 = exp(xq_t[1]))
F_gb2 <- cbind(F_gb2, 1)

pi_prob <- F_gb2[2:(KK + 1)] - F_gb2[1:KK]

if (any(pi_prob < 0)) {pi_prob[pi_prob < 0] <- 0}

pi_prob <- log(pi_prob)

pi_prob <- t(pi_prob)*y
w <- sum(pi_prob)


sprintf


    help_string <- paste0(rep("%.2f", times = 2), collapse = " ")
    cat(sprintf(paste0("true value     ", help_string),
                       true_vals))

    # cat(sprintf(paste0("running means  ", help_string,
    #                    .colMeans(current_pars))))


string_helper <- paste0(rep("%.2f", times = dim_all), collapse = " ")
string_format <- paste0("true value     ", string_helper)

args_print <- c(list(fmt = string_print), test_val)

cat(do.call(sprintf, args = args_print))


args_test <- c("baz", "foob")
do.call(file.path, as.list(c("/foo/bar",args_test)))
file.path("/foo/bar", "baz", "foob")




args <- list(na.rm = TRUE, trim = 0.25)
x <- rnorm(100)
y <- rnorm(100)
a <- quo(mean(x, !!! args))
eval_tidy(a)


lapply(list(x, y), "[", 10)
lapply(!!! pref_var_list, )



# AIM:
# take countries and years from regressor dataset and check for corresponding
# countries and years in the dependent variable dataset!
#
# I. Check if there are differences in observed years among the countries;
# generate list with country names and years for which each country is observed
#
# FIRST: regressor variables dataset
matching_list_reg <- generate_match_list_country_year(data_reg)
check_year_diff <- Reduce(identical_value, matching_list_reg$year)
if (is.logical(check_year_diff)) {
  stop(paste("Years vary with countries in the",
             crayon::red(REGRESSOR),
             "variables dataset!")
  )
} else {
  matching_list_reg$year <- c(matching_list_reg$year[[1]])
  reg_countries <- matching_list_reg$country
  reg_year <- matching_list_reg$year
}
#
# Second: dependent variables dataset
matching_list_dep <- generate_match_list_country_year(data_dep)
check_year_diff <- Reduce(identical_value, matching_list_dep$year)
if (is.logical(check_year_diff)) {
  warning(paste("Years vary with countries in the",
                crayon::red("DEPENDENT"),
                "variables dataset!")
  )
  dep_countries <- matching_list_dep$country
  dep_year <- matching_list_dep$year
} else {
  matching_list_dep$year <- c(matching_list_dep$year[[1]])
  dep_countries <- matching_list_dep$country
  dep_year <- matching_list_dep$year
}
#
#
#
#
#
# II.
# Check if country names are identical in dep. dataset and reg. dataset: this is
# a stepwise procedure:
# Step (1) find the differences in country names
# Step (2) check if they are due to different naming conventions e.g. "Korea"
# vs. "Republic of Korea" etc.
# Step (3) for cases under (2), decide for a name and change correspondingly
# Step (4) do the joint set of countries in both datasets!
#
# @(1):
# check for unequal but similar country names
# (e.g. "Iran" vs "Iran, Republic of" or "Korea" vs "Korea, Republic of")
diff_dep_not_reg <- setdiff(dep_countries, reg_countries)
diff_reg_not_dep <- setdiff(reg_countries, dep_countries)

len_diff_reg_not_dep <- length(diff_reg_not_dep)
len_diff_dep_not_reg <- length(diff_dep_not_reg)

partial_matches_reg_not_dep <- rep(list(list()), times = len_diff_reg_not_dep)
for (i in 1:len_diff_reg_not_dep) {
  partial_matches_reg_not_dep[[i]] <- grep(diff_reg_not_dep[i],
                                           dep_countries,
                                           value = TRUE)
}

partial_matches_dep_not_reg <- rep(list(list()), times = len_diff_dep_not_reg)
for (i in 1:len_diff_dep_not_reg) {
  partial_matches_dep_not_reg[[i]] <- grep(diff_dep_not_reg[i],
                                           reg_countries,
                                           value = TRUE)
}
partial_matches <- unlist(lapply(partial_matches_reg_not_dep, length)) != 0
partial_matches_ID_reg <- which(partial_matches)
partial_matches_reg_not_dep[partial_matches_ID_reg]
diff_reg_not_dep[partial_matches_ID_reg]

partial_matches <- unlist(lapply(partial_matches_dep_not_reg, length)) != 0
partial_matches_ID_dep <- which(partial_matches)
partial_matches_dep_not_reg[partial_matches_ID_dep]
diff_dep_not_reg[partial_matches_ID_dep]
#
#
#
#
#
# Step 2.
# select country names and years from the dependepnd variables dataset that are
# used in the regressor variables dataset -> first step of merging; because the
# WIID4 uses several data sources there will be cases where e.g. Botsvana in the
# year=1970 occurs 3 times or so! Hence we decide which observation/data source
# to pick for these type of countries!
matching_list_all <- rep(list(list()), times = 4)
names(matching_list_all) <- c("country", "year_ttl", "year_unq", "source")

matching_list_all$country <- intersect(matching_list_reg$country,
                                       matching_list_dep$country)
len_country <- length(matching_list_all$country)

ID_year <- matching_list_dep$country %in% matching_list_all$country
matching_list_all$year_ttl <- matching_list_dep$year[ID_year]
matching_list_all$year_unq <- lapply(matching_list_all$year_ttl,
                                     unique,
                                     USE.NAMES = FALSE)
len_year_unq <- sapply(matching_list_all$year_unq, length, simplify = TRUE)
matching_list_all$source <-  rep(list(list()), each = len_country)

matching_list_all_2 <- matching_list_all
for (i in 1:len_country) {
  matching_list_all_2$source[[i]] <- filter(data_dep,
                                            country == matching_list_all$country[i],
                                            year %in% matching_list_all$year_unq[[i]])
}
matching_list_all_2$source[81 ]


data_raw_subset_1 <- filter(data_raw_dep,
                            country %in% matching_list_all$country,
                            year %in% matching_list$year)
unique(data_raw_subset_1$country)
unique(data_raw_subset_1$year)
dim(data_raw_subset_1)
#
#
#
#
#












len_country <- length(matching_country_name)
len_year    <- lapply(matching_year_unq, length)









# matching_list <- generate_match_list_country_year(data_raw_subset_5)
# check_year_diff <- Reduce(identical_value, matching_list$year)
# if (is.logical(check_year_diff)) {
#   warning("Years vary with countries in the dependent variables dataset!")
# } else(
#   matching_list$year <- c(matching_list$year[[1]])
# )

# test_data <- group_by(data_raw_subset_1, country, year)
# View(filter(test_data, source == "Research Study"))
#
# data_raw_dep2 <- data_raw_dep
# recoded_var <- recode(data_raw_dep2$country,
#                       Korea, Republic of = names_replacing,
#                       .default = data_raw_dep2$country)
#
# names_country_dep_data2 <- unique(data_raw_dep2$country)
# # eval(parse(text = ID_to_replace))
# names_country_dep_data
# names_country_dep_data2
# # pmatch(unique(data_raw_dep$country), "Korea")
# data_raw_dep$country[data_raw_dep$country == "K"]
#
#
