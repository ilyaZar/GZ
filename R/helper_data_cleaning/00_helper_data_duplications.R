select_var_pref <- function(value, order) {
  # for given value(s) of a variable and preference order of values for this
  # variable, the preferred value of the variable is returned
  ID_select <- order %in% value
  pref_val <- min(which(ID_select))
  return(order[pref_val])
}
get_ordered_vals <- function(data, variable, largest_value = FALSE) {
  # gets ordered values by preference: if largest_value is TRUE, then highest
  # numeric values are preferred; else, the most frequent values (default) are
  # preferred over less frequent
  if (largest_value) {
    ordered_val <- seq(from = max(data[[variable]]), to = min(data[[variable]]))
  } else {
    ordered_val <- names(sort(table(data[[variable]]),
                      decreasing = T))
  }
  if (any(is.na(data[[variable]]))) {
    ordered_val <- c(ordered_val, NA)
  }
  ordered_val
}
pref_order <- function(data,
                       variable,
                       order) {
  # for a given dataset, variable and its preference order, the preferred value
  # of a variable for each country and unique year is returned.
  # Example:
  # Argentina observed at 1960 two times (or less or more often!), the preferred
  # value of the variable  'source' is returned i.e. it is checked whether one
  # source is better than the other given the order and the preferred is
  # returned. Hence lenght of function output value is country x (unique) years!
  enq_var <- enquo(variable)
  data <- data %>% group_by(country, year) %>%
    summarise(var_pref_order = select_var_pref(value = !! enq_var,
                                               order = order))
  var_pref_order <- data$var_pref_order
  var_pref_order
}
get_duplication_var_list <- function(data_double_entry, dep_data_used = TRUE) {
  # given a dataset with duplicated entries e.g. Argentina observed at 1960 two
  # (or more) times, the variable values causing the duplication are returned in
  # the following structure.
  # a list with 3 elments:
  #     - (1.) (unique) country names of the duplicated entries
  #     - (2.) (unique) years at which the duplicated country is observed
  #     - (3.) for each country under (1.) and year under (2.), a named list
  #       * with names corresponding to the variable name where values differ
  #       * the exact entry differences i.e. different values of the variable
  #
  # The output list is called: "DUPLICATION LIST"
  if (nrow(data_double_entry) == 0) {
    # stop("Empty dataset: no duplicated observations passed!")
    warning("Empty dataset: no duplicated observations passed!")
    return(NULL)
  }
  data <- data_double_entry
  if (dep_data_used) {
    # IF DEPENDENT VARIABLES DATASET USED, WE ELIMINATE VARIABLES RELATED TO
    # INCOME AS THEY WILL ALLWAYS DIFFER (e.g. deciles)
    data <- data %>% select(-c(d1, d2, d3, d4, d5, d6, d7 , d8, d9, d10,
                               q1, q2, q3, q4, q5,
                               id, gini_reported, c2, c3, top5, bottom5,
                               mean, median, mean_usd, median_usd, currency))
  }

  data_info_preproc <- preprocess_data(data_double_entry)
  name_country <- data_info_preproc[[2]][[1]]
  len_country  <- data_info_preproc[[2]][[2]]
  unq_year     <- data_info_preproc[[3]][[1]]
  len_year     <- data_info_preproc[[3]][[2]]

  duplication_list <- rep(list(list()), times = len_country)
  for (i in 1:len_country) {
    duplication_list[[i]] <- rep(list(list()), times = len_year[i])
    for (j in 1:len_year[i]) {
      duplication_list[[i]][[j]] <- get_duplication_var(data = data,
                                                        country = name_country[[i]],
                                                        year = unq_year[[i]][j]
      )
    }
  }
  duplication_list <- list(country_duplicated = name_country,
                           years_duplicated = unq_year,
                           variables_duplicated = duplication_list)
  duplication_list
}
get_duplication_var <- function(data, country, year,
                                 drop_equal_vars = TRUE) {
  # returns duplicated values in all variables for a particular country and year
  enq_country <- enquo(country)
  enq_year    <- enquo(year)

  double_entries <- data %>% filter(country == !! enq_country,
                                    year == !! enq_year)
  len_var <- ncol(double_entries)

  results <- rep(list(list()), each = len_var)
  names(results) <- names(double_entries)

  for (i in 1:len_var) {
    check_entry <- double_entries[, i, drop = TRUE]
    check_equal <- identical(Reduce(get_identical_value2, check_entry),
                             double_entries[1, i, drop = TRUE])
    # all(check_entry[1] == check_entry) only works with numeric values!
    if (check_equal) {
      results[[i]] <- TRUE
    } else {
      results[[i]] <- double_entries[, i, drop = TRUE]
    }
  }

  if (drop_equal_vars) {
    ID_drop <- which(sapply(results, FUN = isTRUE), useNames = FALSE)
    if (identical(length(ID_drop), len_var)) {
      return("No duplication: this observation is unique!")
    }
    return(results[-ID_drop])
  } else {
    return(results)
  }
}
get_duplication_obs <- function(data, pref_var_list = NULL) {
  # finds duplications i.e. repeated observations of a country at a year; the
  # output is a tibble with observations at least twice e.g. Argentina in 1960
  # observed two (or more) times: there are two parts; if pre_var_list == NULL,
  # then part I. is executed to save and return duplicated observations . Else,
  # part II. is run, where an additional `filter()` step to retrieve the
  # preferred value of a variable is run and the "remaining" duplicated observ.
  # are saved and returned i.e. the duplications after the preferred value of
  # a variable is already selected!

  # len_filter_vars <- ncol(pref_var_list)
  # var_type <- vapply(pref_var_list, class, character(1))
  # var_chars <- unlist(var_type) == "character"

  data_info_preproc <- preprocess_data(data)
  name_country <- data_info_preproc[[2]][[1]]
  len_country  <- data_info_preproc[[2]][[2]]
  unq_year     <- data_info_preproc[[3]][[1]]

  duplication_data <- create_empty_tibble(data = data)

  if (is.null(pref_var_list)) {
    # I. IF NO PREFERRED VARIABLE LIST IS PASSED: JUST CHECK FOR DUPLICATED OBS!
    for (i in 1:len_country) {
      len_unq_year <- length(unq_year[[i]])
      ID_var <- length(unlist(unq_year[1:i])) - len_unq_year
      for (j in 1:len_unq_year) {

        repeated_observations <- data %>%
          filter(country == name_country[i],
                 year == unq_year[[i]][j])

        if (nrow(repeated_observations) >= 2) {
          duplication_data <- rbind(duplication_data, repeated_observations)
        }
      }
    }
  } else {
    # II. IF PREFERRED VARIABLE LIST IS PASSED: CHECK FOR DUPLICATED OBS. AFTER
    # FILTERING A VARIABLE WITH ITS PREFERRED VALUE!
    filter_vars <- names(pref_var_list)

    pref_var_list <- as.data.frame(pref_var_list, stringsAsFactors = FALSE)

    for (i in 1:len_country) {
      len_unq_year <- length(unq_year[[i]])
      ID_var <- length(unlist(unq_year[1:i])) - len_unq_year
      for (j in 1:len_unq_year) {
        ID_var <- ID_var + 1

        var_expr_char <- get_filter_expr(filter_vars, pref_var_list[ID_var, ])

        repeated_observations <- data %>%
          filter(country == name_country[i],
                 year == unq_year[[i]][j]) %>%
          filter(eval_tidy(parse_expr(var_expr_char)))

        if (nrow(repeated_observations) >= 2) {
          duplication_data <- rbind(duplication_data, repeated_observations)
        }
      }
    }
  }
  duplication_data
}
get_filter_expr <- function(lhs_var, rhs_val, connector = " & ") {
  # constructs expression used for filtering preferred variable values
  len_expr <- length(lhs_var)
  # INPUT CHECKS
  if (length(lhs_var) != length(rhs_val)) {
    stop("Filter expression invalid: rhs-variables do not match lhs-values")
  }
  if (length(lhs_var) > 1) {
    warning("Filter expression contains multiple conditions with connection: ",
            connector, ". Connector may be too restrictive -> check for dropped
            country x years!")
  }
  # EXPRESSION COMPUTATION
  final_expr <- character(0)
  for (i in 1:len_expr) {
    if (is.na(rhs_val[[i]])) {
      var_expr <- paste0("is.na(", lhs_var[i],
                         ")"
                         )
    } else if (is.character(rhs_val[[i]])) {
      char_val <- paste0("'", rhs_val[[i]], "'")
      var_expr <- paste0(lhs_var[i],
                         " == ",
                         char_val
                         )
    } else if (is.numeric(rhs_val[[i]])) {
      var_expr <- paste0(lhs_var[i],
                         " == ",
                         rhs_val[[i]]
                         )
    } else {
      stop("Unable to construct filter expression: unknown type! Either char or
         double required!")
    }
    final_expr <- c(final_expr, var_expr)
  }
  final_expr <- paste(final_expr, collapse = " & ")
  # if (any(vapply(rhs_val, is.na, logical(1)))) {
  #   var_expr <- paste0("is.na(", lhs_var,
  #                      ")",
  #                      collapse = connector)
  #   return(var_expr)
  # } else if (any(vapply(rhs_val, is.character, logical(1)))) {
  #   rhs_val <- paste0("'", rhs_val, "'")
  #   var_expr <- paste0(lhs_var,
  #                      " == ",
  #                      rhs_val,
  #                      collapse = connector)
  #   return(var_expr)
  # } else if (any(vapply(rhs_val, is.double, logical(1)))) {
  #   var_expr <- paste0(lhs_var,
  #                      " == ",
  #                      rhs_val,
  #                      collapse = connector)
  #   return(var_expr)
  # } else {
  #   stop("Unable to construct filter expression: unknown type! Either char or
  #        double required!")
  # }
}
report_duplicate_var_all <- function(duplication_list) {
  # given a duplication list obtained obtained from the function
  # "get_duplication_var_list()", for each country and year, all variables causing
  # the duplications are printed
  country           <- duplication_list[[1]]
  years             <- duplication_list[[2]]
  list_dupl_entries <- duplication_list[[3]]


  len_country <- length(country)
  len_years <- lapply(years, length)

  for (i in 1:len_country) {
    for (j in 1:len_years[[i]]) {
      cat(crayon::red(country[i]),
          "observed in",
          crayon::green(years[[i]][j]),
          ":\n",
          crayon::cyan(names(list_dupl_entries[[i]][[j]])),
          "\n")
    }
  }
}
report_duplicate_var_frq <- function(duplication_list) {
  # given a duplication list obtained obtained from the function
  # "get_duplication_var_list()", only the frequencies of values causing the
  # duplications for all countries and years; this is usefull to decide which
  # to use for eliminating redundant oberservations (duplicated observations)
  country           <- duplication_list[[1]]
  years             <- duplication_list[[2]]
  list_dupl_entries <- duplication_list[[3]]

  len_country <- length(country)
  len_years <- lapply(years, length)

  list_dupl_all <- character()
  for (i in 1:len_country) {
    for (j in 1:len_years[[i]]) {
      list_dupl_all <- c(list_dupl_all, names(list_dupl_entries[[i]][[j]]))
    }
  }
  frq_dupl <- table(list_dupl_all)

  frq_dupl
}
reduce_by_preference <- function(data, var_name, pref_order) {
  # reduce dataset by preferred value for a specific variable: the preferred
  # order must be of dimension country x (unique) years, so for duplicated
  # observations the filter step: filter(eval_tidy(parse_expr(var_expr_char)))
  # generates the preferred observation. NOTE: non-duplicated (unique)
  # observations are not eliminated since there preferred value is unique too
  # and this is takes care of by having pref_order of dimension
  # country x (unique) years!
  data_info_preproc <- preprocess_data(data)
  name_country <- data_info_preproc[[2]][[1]]
  len_country  <- data_info_preproc[[2]][[2]]
  unq_year     <- data_info_preproc[[3]][[1]]
  len_year     <- data_info_preproc[[3]][[2]]

  preferred_data_all <- create_empty_tibble(data = data)

  for (i in 1:len_country) {
    len_unq_year <- length(unq_year[[i]])
    ID_var <- length(unlist(unq_year[1:i])) - len_unq_year
    for (j in 1:len_year[[i]]) {
      ID_var <- ID_var + 1
      var_expr_char <-  get_filter_expr(var_name, pref_order[ID_var])
      preferred_data <- data %>% filter(country == name_country[i],
                                         year == unq_year[[i]][j]) %>%
          filter(eval_tidy(parse_expr(var_expr_char)))
      preferred_data_all <- rbind(preferred_data_all, preferred_data)
    }
  }
  preferred_data_all
}
