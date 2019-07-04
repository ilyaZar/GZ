countries_JLP13_bench_reg <- unique(data_all$country)
num_JL13_bench_reg <- length(countries_JLP13_bench_reg)
results_prelim_JL13_bench_reg <- results_prelim_all
results_prelim_JL13_bench_reg[, "year"] <- lubridate::ymd(as.numeric(paste0(results_prelim_JL13_bench_reg$year, "01", "01")))
missings_JL13_bench_reg <- results_prelim_JL13_bench_reg %>% filter(is.na(a)) %>% select(country, year)

plot_pars_ts <- rep(list(list()), times = num_JL13_bench_reg)
for (i in 1:num_JL13_bench_reg) {
  plot_pars_ts[[i]] <- generate_ts_plot_country(data = results_prelim_JL13_bench_reg,
                                                country_name = countries_JLP13_bench_reg[i],
                                                nonconv_obs = missings_JL13_bench_reg)
  for (j in 1:3) {
    name <- paste0("results/analyse_data_plots/JLP13_bench_reg/",
                   countries_JLP13_bench_reg[i], "_", j, ".pdf")
    ggsave(name, plot = plot_pars_ts[[i]][[j]])
  }
}
