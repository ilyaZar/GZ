replace_var <- function(data, var_name, entry_old, entry_new) {
  # replacing/recoding entries for a particular variable (column) in dataset
  col_replace <- data[[var_name]] == entry_old
  data[col_replace, ][[var_name]] <- entry_new
  data
  # data_col <- paste("data", "$", var_name, sep = "")
  # data_col[data_col == entry_old] <- entry_new
}
get_identical_value <- function(x,y) {
  # checking two arguments for identical value; return identical value or FALSE
  # -> usefull in combination with Reduce(): sequence of euqal values or FALSE
  if (identical(x, y)) x else FALSE
}
get_identical_value2 <- function(x,y) {
  # checking two arguments for identical value; return identical value or NULL
  # -> usefull in combination with Reduce(): sequence of euqal values or NULL
  if (identical(x, y)) x else NULL
}
preprocess_data <- function(data) {
  # Helper function for pre-processing repeated tasks. In many helper functions,
  # particlur data information is necessary; this includes
  #   - dim()
  #   - length of unique values of country variable
  #   - length of unique values of years
  # Hence, this function returns them as a list containing
  dim_data <- dim(data)
  name_var <- names(data)
  len_var  <- length(name_var)
  name_country <- unique(data$country)
  len_country  <- length(name_country)

  year_list_unq <- rep(list(list()), times = len_country)
  for (i in 1:len_country) {
    year_list_unq[[i]] <- data %>% filter(country == name_country[i]) %>%
      .$year %>% unique()
  }
  len_year_list_unq <- sapply(year_list_unq, length)


  year_list_all <- rep(list(list()), times = len_country)
  for (i in 1:len_country) {
    year_list_all[[i]] <- data %>% filter(country == name_country[i]) %>%
      .$year
  }
  len_year_list_all <- sapply(year_list_all, length)

  preprocessed_list <- list(dim = dim_data,
                            country = list(name_country, len_country),
                            year_unq = list(year_list_unq, len_year_list_unq),
                            year_all = list(year_list_all, len_year_list_all),
                            vars = list(name_var, len_var))
  preprocessed_list
}
create_empty_tibble <- function(data) {
  # creates an empty tibble with column names (number of variables) specified in
  # the dataset passed to "data"
  col_names <- as.list(paste0(sapply(data, class), "()"))
  col_types <- lapply(col_names, function(x) {eval(parse(text = x))})
  names(col_types) <- names(data)

  empty_tibble <- tibble(!!! col_types)
  empty_tibble
}
generate_match_list_country_year <- function(data, unique_years = FALSE) {

  data_info_preproc <- preprocess_data(data)
  country_list      <- data_info_preproc[[2]][[1]]
  if (unique_years) {
    year_list <- data_info_preproc[[3]][[1]]
  } else {
    year_list <- data_info_preproc[[4]][[1]]
  }

  match_list <- list(country = country_list, year = year_list)
  match_list
}
