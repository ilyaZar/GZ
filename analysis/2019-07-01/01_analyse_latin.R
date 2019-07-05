latin_south_subset <- c("Argentina",
                        "Bolivia",
                        "Brazil",
                        "Chile",
                        "Colombia",
                        "Costa Rica",
                        "Dominican Republic",
                        "Ecuador",
                        "El Salvador",
                        "Guatemala",
                        "Haiti",
                        "Honduras",
                        "Mexico",
                        "Nicaragua",
                        "Panama",
                        "Paraguay",
                        "Peru",
                        "Uruguay",
                        "Venezuela")
num_latin <- length(latin_south_subset)
results_prelim_latin <- filter(results_prelim_all, country %in% latin_south_subset)
starting_values_prelim_latin <- filter(results_starting_values_all, country %in% latin_south_subset)
# View(results_prelim_latin)
results_prelim_latin2 <- results_prelim_latin
starting_values_prelim_latin2 <- starting_values_prelim_latin
results_prelim_latin2[, "year"] <- lubridate::ymd(as.numeric(paste0(results_prelim_latin2$year, "01", "01")))
starting_values_prelim_latin2[, "year"] <- lubridate::ymd(as.numeric(paste0(starting_values_prelim_latin2$year, "01", "01")))
missings_latin <- results_prelim_latin2 %>% filter(is.na(a)) %>% select(country, year)

plot_pars_ts <- rep(list(list()), times = num_latin)
for (i in 1:length(latin_south_subset)) {
  plot_pars_ts[[i]] <- generate_ts_plot_country(data = results_prelim_latin2,
                                                starting_values = starting_values_prelim_latin2,
                                                country_name = latin_south_subset[i],
                                                nonconv_obs = missings_latin)
  for (j in 1:3) {
    name <- paste0("results/analyse_data_plots/latin2/",
                   latin_south_subset[i], "_", j, ".pdf")
    ggsave(name, plot = plot_pars_ts[[i]][[j]])
  }
}
generate_ts_plot_country(data = results_prelim_latin2,
                         starting_values = starting_values_prelim_latin2,
                         country_name = latin_south_subset[5],
                         nonconv_obs = missings_latin)
