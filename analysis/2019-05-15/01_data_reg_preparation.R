## @knitr reg_country_sub
adv_econ_subset <- c("Australia", "Austria", "Belgium", "Canada", "Denmark",
                     "Finland", "France",  "Germany", "Ireland", "Israel",
                     "Italy", "Japan", "Korea", "Netherlands",  "Norway",
                     "Singapore", "Spain", "Sweden", "United Kingdom",
                     "United States")
dev_econ_subset <- c("Argentina", "Bangladesh", "Bolivia", "Brazil", "Chile",
                     "China", "Costa Rica", "Ecuador", "Egypt", "El Salvador",
                     "Ghana", "Guatemala", "Honduras", "India", "Indonesia",
                     "Iran", "Kenya", "Malaysia", "Mexico", "Pakistan",
                     "Panama", "Paraguay", "Peru", "Philippines", "Sri Lanka",
                     "Thailand", "Turkey", "Uganda", "Uruguay", "Venezuela",
                     "Zambia")
sub_country_reg_jlp13 <- sort(c(adv_econ_subset, dev_econ_subset))
## @knitr regions_povcal_classification
region_povcal_europe_central_asia <- c("Turkey")
region_povcal_south_asia <- c("Bangladesh",  "India", "Pakistan",  "Sri Lanka")
region_povcal_east_asia_pacific <- c("China", "Indonesia", "Malaysia",
                                     "Philippines", "Thailand")
region_povcal_latin_america_caribbean <- c("Argentina", "Bolivia", "Brazil",
                                           "Chile", "Costa Rica", "Ecuador",
                                           "El Salvador", "Guatemala",
                                           "Honduras","Mexico", "Panama",
                                           "Paraguay", "Peru", "Uruguay",
                                           "Venezuela")
region_povcal_middle_east_north_africa <- c("Egypt", "Iran")
region_povcal_middle_sub_saharan_africa <- c("Ghana","Kenya", "Uganda",
                                             "Zambia")
regions_povcal_all <- list(region_povcal_east_asia_pacific,
                           region_povcal_europe_central_asia,
                           region_povcal_latin_america_caribbean,
                           region_povcal_middle_east_north_africa,
                           region_povcal_middle_sub_saharan_africa,
                           region_povcal_south_asia)
regional_coverage <- length(unlist(regions_povcal_all))
## @knitr data_dev_adv
len_dev_econ_subset <- length(dev_econ_subset)
len_adv_econ_subset <- length(adv_econ_subset)
len_tot_econ_subset <- length(sub_country_reg_jlp13)
cat("THERE IS A TOTAL of",
    crayon::red(len_dev_econ_subset),
    crayon::green("developing countries"),
    "in JLP13!\n")
cat("THERE IS A TOTAL of",
    crayon::red(len_adv_econ_subset),
    crayon::green("developed countries"),
    "in JLP13!\n")
cat("THERE IS A TOTAL of",
    crayon::red(len_tot_econ_subset),
    crayon::green("developed & developing countries"),
    "in JLP13!\n")
cat("OF THE",
    crayon::red(len_dev_econ_subset),
    "DEVELOPING COUNTRIES,",
    regional_coverage,
    "CAN BE CLASSIFIED WITH THE POVCAL REGIONAL CLASSIFICATION SCHEME!")
## @knitr trade_vars
# These are the trade openess variables:
reg_var_trade_open <- c("open",
                        "open_sh_med",
                        "open_sh_max",
                        "impdev_y",
                        "expA_y",
                        "expM_y",
                        "tariff",
                        "tariff_sh_max",
                        "tariff_sh_med")
## @knitr finan_vars
# These are the financial openess variables:
reg_var_fin_open <- c("finopen",
                      "liab_y",
                      "fdil_y",
                      "debtl_y",
                      "equityl_y",
                      "finopen_sh_max81",
                      "finopen_sh_med81",
                      "kaopen")
## @knitr macro_vars
# These are other macro variables:
reg_var_macro <- c("ngdp",
                   "ngdpd",
                   "pop")
## @knitr control_vars
# These are the control variables:
reg_var_controls <- c("pcrdbofgdp",
                      "ls15_bl",
                      "lh15_bl",
                      "tyr15_bl",
                      "empsh_agr",
                      "empsh_ind")
## @knitr instrument_vars
# These are the instrument variables:
reg_var_instruments <- c("rfdia_adv")
## @knitr define_reg
reg_var_full <- c(reg_var_trade_open,
                  reg_var_fin_open,
                  reg_var_macro,
                  reg_var_controls,
                  reg_var_instruments)
reg_var_sub_JLP13_bench <- c("open","expA_y", "tariff", "finopen", "pcrdbofgdp", "empsh_ind")
## @knitr select_reg
data_reg_full <- data_raw_reg %>% select(country, year, !!! reg_var_full) %>%
  drop_na(!!! reg_var_full)
data_reg_sub_JLP13_bench <- data_raw_reg %>% select(country, year, !!! reg_var_sub_JLP13_bench) %>%
  drop_na(!!! reg_var_sub_JLP13_bench)
generate_data_info(data = "data_reg_full", data_compare = "data_reg_sub_JLP13_bench")
## @knitr select_reg_country_jlp13
data_reg_full_jlp13 <- data_reg_full %>%
  filter(country %in% sub_country_reg_jlp13)
data_reg_sub_JLP13_bench_jlp13 <- data_reg_sub_JLP13_bench %>%
  filter(country %in% sub_country_reg_jlp13)
generate_data_info(data = "data_reg_full_jlp13", data_compare = "data_reg_sub_JLP13_bench_jlp13")
## @knitr duplicate_check
# duplicated_reg_data <- get_duplication_var_list(data_reg_full_jlp13,
#                                                 dep_data_used = FALSE)
# report_duplicate_var_all(duplicated_reg_data)
data_dupl_regs <- get_duplication_obs(data = data_reg_full_jlp13)
print(data_dupl_regs)
## @knitr final_reg_data
# data_reg <- data_reg_full
# data_reg <- data_raw_reg %>% select(country, year)
data_reg <- data_reg_sub_JLP13_bench
# data_reg <- data_reg_full_jlp13
# data_reg <- data_reg_sub_JLP13_bench_jlp13

