## @knitr get_match_lists
# FIRST: regressor variables dataset
matching_list_reg <- generate_match_list_country_year(data_reg)
reg_countries <- matching_list_reg$country
# reg_year <- matching_list_reg$year
# SECOND: dependent variables dataset
matching_list_dep <- generate_match_list_country_year(data_dep)
dep_countries <- matching_list_dep$country
# dep_year <- matching_list_dep$year
## @knitr check_similar_names
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
## @knitr use_joint_names
data_dep <- replace_var(data_dep, var_name = "country",
                        entry_old = "Korea, Republic of",
                        entry_new = diff_reg_not_dep[partial_matches_ID_reg])
## @knitr merge_datasets
data_all <- inner_join(data_dep, data_reg)
## @knitr report_final_datasets
generate_data_info("data_all", details_miss = TRUE, details_avail = TRUE)
# THE REGRESSORS USED IN THE DATASET ARE:
names(data_all)
