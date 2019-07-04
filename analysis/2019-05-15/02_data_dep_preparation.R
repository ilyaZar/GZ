## @knitr clean_decile
var_deciles <- paste0("d", 1:10)
data_raw_dep_subset_I <-  data_raw_dep %>%
  drop_na(!!! var_deciles) %>%
  drop_na(mean_usd) %>% # or mean (which is the mean in local currency)
  drop_na(gini_reported) %>%
  drop_na(population)
# Checking if previous command indeed clears for missings (NA) in:
check_missing_var(data = "data_raw_dep",
                  name_var =  paste0("d", 1:10),
                  data_name = "raw version of the dependent variables")
check_missing_var(data = "data_raw_dep_subset_I",
                  name_var =  paste0("d", 1:10),
                  data_name = "data_raw_dep_subset_I")
## @knitr countries_dropped
missing_decile_country <- setdiff(unique(data_raw_dep$country),
                                  unique(data_raw_dep_subset_I$country))
print(missing_decile_country)
## @knitr income_definition
data_raw_dep_subset_II <- select(data_raw_dep_subset_I,
                                 country, year,
                                 d1, d2, d3, d4, d5,
                                 d6, d7, d8, d9, d10,
                                 starts_with("resource"),
                                 starts_with("scale"),
                                 ends_with("_unit"),
                                 starts_with("qualit"),
                                 starts_with("source"),
                                 everything())
table(data_raw_dep_subset_II$resource)
table(filter(data_raw_dep_subset_II,
             resource == "Income (gross)")$resource_detailed)
table(filter(data_raw_dep_subset_II,
             resource == "Income (net)")$resource_detailed)
table(filter(data_raw_dep_subset_II,
             resource == "Income (net/gross)")$resource_detailed)
table(data_raw_dep_subset_II$reference_unit)
table(data_raw_dep_subset_II$sharing_unit)
## @knitr subset_income_defintion
data_raw_dep_subset_III <- data_raw_dep_subset_II  %>%
  filter(resource %in% c("Income (net)", "Income (net/gross)"),
         reference_unit == "Person")
## @knitr source_preferred
precedence_source <- get_ordered_vals(data = data_raw_dep_subset_III,
                               variable = "source")
preferred_source <- pref_order(data = data_raw_dep_subset_III,
                               variable = source,
                               order = precedence_source)
## @knitr duplicates_after_source
data_dupl_source <- get_duplication_obs(data = data_raw_dep_subset_III,
                                         list(source = preferred_source))
duplicates_source <- get_duplication_var_list(data_dupl_source)
data_source_reduced <- reduce_by_preference(data_raw_dep_subset_III,
                                            "source",
                                            preferred_source)
generate_data_info("data_raw_dep_subset_III", "data_source_reduced")
## @knitr scale_preferred
# report_duplicate_var_all(duplicates_source)
sort(report_duplicate_var_frq(duplicates_source), decreasing = TRUE)
precedence_scale <- get_ordered_vals(data = data_source_reduced,
                              variable = "scale")
preferred_scale <- pref_order(data = data_source_reduced,
                              variable = scale,
                              order = precedence_scale)
## @knitr duplicates_after_scale
data_dupl_scale <- get_duplication_obs(data = data_source_reduced,
                                        list(scale = preferred_scale))
duplicates_scale <- get_duplication_var_list(data_dupl_scale)
data_scale_reduced <- reduce_by_preference(data_source_reduced,
                                           "scale",
                                           preferred_scale)
generate_data_info("data_source_reduced", "data_scale_reduced")
## @knitr areacovr_preferred
# report_duplicate_var_all(duplicates_scale)
sort(report_duplicate_var_frq(duplicates_scale), decreasing = TRUE)
precedence_areacovr <- get_ordered_vals(data = data_scale_reduced,
                                variable = "areacovr")
preferred_areacovr <- pref_order(data = data_scale_reduced,
                                 variable = areacovr,
                                 order = precedence_areacovr)
## @knitr duplicates_after_area_covr
data_dupl_areacovr <- get_duplication_obs(data = data_scale_reduced,
                                          list(areacovr = preferred_areacovr))
duplicates_areacovr <- get_duplication_var_list(data_dupl_areacovr)
data_areacovr_reduced <- reduce_by_preference(data_scale_reduced,
                                              "areacovr",
                                              preferred_areacovr)
generate_data_info("data_scale_reduced", "data_areacovr_reduced")
data_final_reduced <- data_areacovr_reduced
# ## @knitr source_comments_preferred
# # report_duplicate_var_all(duplicates_areacovr)
# sort(report_duplicate_var_frq(duplicates_areacovr), decreasing = TRUE)
# precedence_source_comments <- get_ordered_vals(data = data_areacovr_reduced,
#                                         variable = "source_comments")
# preferred_source_comments <- pref_order(data = data_areacovr_reduced,
#                                         variable = source_comments,
#                                         order = precedence_source_comments)
# ## @knitr duplicates_after_source_comments
# data_dupl_source_comments <- get_duplication_obs(data = data_areacovr_reduced,
#                                                  list(source_comments = preferred_source_comments))
# duplicates_source_comments <- get_duplication_var_list(data_dupl_source_comments)
#
# report_duplicate_var_all(duplicates_source_comments)
# sort(report_duplicate_var_frq(duplicates_source_comments), decreasing = TRUE)
# data_source_comments_reduced <- reduce_by_preference(data_areacovr_reduced,
#                                                      "source_comments",
#                                                      preferred_source_comments)
# generate_data_info("data_areacovr_reduced", "data_source_comments_reduced")
# ## @knitr current_data_problems
# report_duplicate_var_all(duplicates_source_comments)
# data_probs <- select(data_dupl_source_comments, country, year,scale_detailed,
#                      quality_score, popcovr_detailed)
# ## @knitr cleaning_remainings
# precedence_scale_detailed <- get_ordered_vals(data = data_source_comments_reduced,
#                                               variable = "scale_detailed")
# precedence_quality_score <- get_ordered_vals(data = data_source_comments_reduced,
#                                              variable = "quality_score",
#                                              largest_value = TRUE)
# precedence_popcovr_detailed <- get_ordered_vals(data = data_source_comments_reduced,
#                                                 variable = "popcovr_detailed")
# preferred_scale_detailed <- pref_order(data = data_source_comments_reduced,
#                                        variable = scale_detailed,
#                                        order = precedence_scale_detailed)
# preferred_quality_score <- pref_order(data = data_source_comments_reduced,
#                                       variable = quality_score,
#                                       order = precedence_quality_score)
# preferred_popcovr_detailed <- pref_order(data = data_source_comments_reduced,
#                                          variable = popcovr_detailed,
#                                          order = precedence_popcovr_detailed)
#
# data_dupl_final <- get_duplication_obs(data = data_source_comments_reduced,
#                                        list(scale_detailed = preferred_scale_detailed,
#                                             quality_score = preferred_quality_score,
#                                             popcovr_detailed = preferred_popcovr_detailed))
# duplicates_final <- get_duplication_var_list(data_dupl_final)
# data_final_reduced <- data_source_comments_reduced %>%
#   reduce_by_preference("scale_detailed", preferred_scale_detailed)  %>%
#   reduce_by_preference("quality_score", preferred_quality_score) %>%
#   reduce_by_preference("popcovr_detailed", preferred_popcovr_detailed)
#
# generate_data_info("data_source_comments_reduced", "data_final_reduced")

data_dep <- select(data_final_reduced, country, year,
                   d1, d2, d3, d4, d5,
                   d6, d7, d8, d9, d10,
                   population, gini_reported,
                   mean, mean_usd)
# View(data_dep)
