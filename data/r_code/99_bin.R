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
