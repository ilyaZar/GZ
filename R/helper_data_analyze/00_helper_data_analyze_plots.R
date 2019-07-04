generate_ts_plot_country <- function(data,
                                     country_name,
                                     nonconv_obs) {
  # browser()
  nonconv_obs_sub <- nonconv_obs %>% filter(country == country_name)
  data_sub <- filter(data, country == country_name)
  len_sub  <- nrow(data_sub)
  gini_emp <- numeric(len_sub)
  expt_emp <- numeric(len_sub)
  var_emp  <- numeric(len_sub)
  skew_emp <- numeric(len_sub)
  kurt_emp <- numeric(len_sub)

  for (i in 1:len_sub) {
    if (is.na(data_sub$a[i])) {
      gini_emp[i] <- NA
      expt_emp[i] <- NA
      var_emp[i]  <- NA
      skew_emp[i] <- NA
      kurt_emp[i] <- NA
    } else {
      m1 <- GB2::moment.gb2(k = 1,
                            shape1 = data_sub$a[i],
                            scale  = data_sub$b[i],
                            shape2 = data_sub$p[i],
                            shape3 = data_sub$q[i])
      m2 <- GB2::moment.gb2(k = 2,
                            shape1 = data_sub$a[i],
                            scale  = data_sub$b[i],
                            shape2 = data_sub$p[i],
                            shape3 = data_sub$q[i])
      m3 <- GB2::moment.gb2(k = 3,
                            shape1 = data_sub$a[i],
                            scale  = data_sub$b[i],
                            shape2 = data_sub$p[i],
                            shape3 = data_sub$q[i])
      m4 <- GB2::moment.gb2(k = 4,
                            shape1 = data_sub$a[i],
                            scale  = data_sub$b[i],
                            shape2 = data_sub$p[i],
                            shape3 = data_sub$q[i])


      gini_emp[i] <- GB2::gb2.gini(shape1 = data_sub$a[i],
                                   shape2 = data_sub$p[i],
                                   shape3 = data_sub$q[i])$Gini
      expt_emp[i] <- m1
      var_emp[i]  <- m2 - m1^2
      skew_emp[i] <- (m3 - 3*m1*var_emp[i] - m1^3)/var_emp[i]^(3/2)
      kurt_emp[i] <- m4 - 4*m1*m3 + 6*m1^2*m2 - 3*m1^4
    }
  }
  data_sub <- cbind(data_sub,
                    gini_emp = gini_emp,
                    expt_emp = expt_emp,
                    var_emp  = var_emp,
                    sd_emp   = sqrt(var_emp),
                    skew_emp = skew_emp,
                    kurt_emp = kurt_emp)

  data_all_years  <-  data.frame(year = seq.Date(from = min(data_sub$year),
                                                 to = max(data_sub$year),
                                                 by = "year"))
  data_complete <- left_join(data_all_years,
                             data_sub,
                             by = "year")
  data_nonconvs <- data_complete %>% filter(country %in% nonconv_obs_sub$country &
                                            year %in% nonconv_obs_sub$year) %>%
    replace_na(replace = list(a = 0, b = 0, p = 0, q = 0,
                              gini_emp = 0,
                              expt_emp = 0,
                              var_emp  = 0,
                              sd_emp   = 0,
                              skew_emp = 0,
                              kurt_emp = 0))

  data_nonobs <- data_complete %>% filter(is.na(a) &
                                            !(year %in% data_nonconvs$year)) %>%
    replace_na(replace = list(a = 0, b = 0, p = 0, q = 0,
                              gini_reported = 0,
                              y_mean = 0))

  plot_a <- ggplot(data_complete, aes(year, a)) +
    geom_point() +
    geom_line() +
    geom_point(data = data_nonconvs, mapping = aes(x = year, y = a), col = "red") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = a), col = "blue")

  plot_b <- ggplot(data_complete, aes(year, b)) +
    geom_point() +
    geom_line() +
    geom_point(data = data_nonconvs, mapping = aes(x = year, y = b), col = "red") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = b), col = "blue")

  plot_p <- ggplot(data_complete, aes(year, p)) +
    geom_point() +
    geom_line() +
    geom_point(data = data_nonconvs, mapping = aes(x = year, y = p), col = "red") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = p), col = "blue")

  plot_q <- ggplot(data_complete, aes(year, q)) +
    geom_point() +
    geom_line() +
    geom_point(data = data_nonconvs, mapping = aes(x = year, y = q), col = "red") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = q), col = "blue")

  # browser()

  plot_y <- ggplot(data_complete, aes(year, y_mean)) +
    geom_point() +
    geom_line() +
    geom_line(mapping = aes(year, y_mean_usd),col = "green") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = y_mean), col = "blue") +
    ylab("income mean (green = USD)")

  plot_gini <- ggplot(data_complete, aes(year, gini_reported)) +
    geom_point() +
    geom_line(col = "blue") +
    geom_line(mapping = aes(x = year, y = gini_emp), col = "red") +
    geom_point(data = data_nonconvs, mapping = aes(x = year, y = gini_emp), col = "red") +
    geom_point(data = data_nonobs, mapping = aes(x = year, y = gini_reported), col = "blue") +
    ylab("Gini (reported - blue, estimated - red)")


  plot_expt <- ggplot(data_complete, aes(year, expt_emp)) +
    geom_point() +
    geom_line() +
    ylab("mean")

  plot_var <- ggplot(data_complete, aes(year, var_emp)) +
    geom_point() +
    geom_line() +
    ylab("variance")

  # plot_sd <- ggplot(data_complete, aes(year, sd_emp)) +
  #   geom_point() +
  #   geom_line()

  plot_skew <- ggplot(data_complete, aes(year, skew_emp)) +
    geom_point() +
    geom_line() +
    ylab("skewness")

  plot_kurt <- ggplot(data_complete, aes(year, kurt_emp)) +
    geom_point() +
    geom_line() +
    ylab("kurtosis")

  plot_all_pars <- gridExtra::grid.arrange(plot_a,
                                           plot_b,
                                           plot_p,
                                           plot_q,
                                           nrow = 2,
                                           top = country_name)
  plot_measures1 <- gridExtra::grid.arrange(plot_y,
                                            plot_gini,
                                            nrow = 2,
                                            top = country_name)
  plot_measures2 <- gridExtra::grid.arrange(plot_expt,
                                            plot_var,
                                            # plot_sd,
                                            plot_skew,
                                            plot_kurt,
                                            nrow = 2,
                                            top = country_name)

  return(list(plot_all_pars, plot_measures1, plot_measures2))
}
