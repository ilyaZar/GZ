# ANALYSIS OF TRADE OPENENESS: --------------------------------------------
# I. get subset of trade openess variables
var_trade <- c("open",
               "open_sh_med",
               "open_sh_max",
               "impdev_y",
               "expA_y",
               "expM_y",
               "tariff",
               "tariff_sh_max",
               "tariff_sh_med")
data_JLP13_trade <- select(data_JLP13, c("country", "year", var_trade))
# TRADE OPENESS: ----------------------------------------------------------
# I. de facto trade openess -> "open"
# 1. for the subset of JLP13 countries: what are the time periods of NAs
# na_open_set <- unique(filter(data_JLP13_trade, is.na(open))$country)
# Number of missings per year
# table(filter(data_JLP13_trade, is.na(open))$year)
# the years 1970 are missing for all 51 countries!
# setdiff(subset_countries, na_open_set)
# But the remaining years do not have any missings!
# Now, chose the number of observations i.e. time periods of the variable "open"
# num_open <- 16
# countries_open <- generate_num_countries(data = data_raw,
#                                          var = "open",
#                                          num_open = num_open)
# countries_num_open <- length(countries_open)

num_ttl <- 5
# num_cns <-
for (i in 1:length(var_trade)) {
  current_var <- var_trade[i]
  # assign(x = paste(na, current_var, sep = "_"), value = char(0))
  current_countries <- generate_countries_num_var(data = data_JLP13_trade,
                                                  var = current_var,
                                                  num = num_ttl)
  out_name <- paste("countries", current_var, sep = "_")
  assign(x = out_name, value = current_countries)
  assign(x = paste(out_name, "diff", sep = "_"), value = current_countries)
}
check_country <- setdiff(subset_countries, countries_tariff_sh_med)
check_country
View(data_JLP13_trade %>% filter(country %in% check_country) %>% select(country, tariff))

test_results <- analyse_var_pattern(data = data_JLP13,
                                    var_list = c("open", "expA_y", "expM_y"),
                                    country_list = "all",
                                    fixing = c("variable"))
