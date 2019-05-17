check_missing_var <- function(data,
                              name_var,
                              data_name) {
  # checking if variable "name_var" is missing; name_var can be a vector of
  # characters specifying several variable to check for missing values (is.na())
  num_var <- length(name_var)
  check <- logical(num_var)
  for (i in 1:num_var) {
    col_expr <- parse(text = paste0(data, "$", name_var[i]))
    check[i] <- any(is.na(eval(col_expr)))
  }
  if (any(check)) {
    msg1 <- paste("There are NA values in the",
                  crayon::red(data_name),
                  "dataset!",
                  "\n",
                  "Affected variable(s):")
    msg2 <- paste(crayon::green(name_var[which(check)]), "\n")
    message(msg1)
    message(msg2)
  } else {
    print(paste("No NA values found in the specified variable:",
                name_var))
    message(paste("The",
                  crayon::green(data_name),
                  "dataset",
                  "does not have any missings in the variable(s):",
                  paste(name_var, collapse = ", ")))
  }
}
generate_data_info <- function(data,
                               data_compare  = NULL,
                               details_miss  = FALSE,
                               details_avail = FALSE) {
  # Generates useful information about the data:
  #   - shows the dimension
  #   - shows the number of unique countries in the dataset
  #   - details about missings if `details_miss = TRUE`:
  #       -> "country xyz is observed from to with missing years at ...."
  #   - details about available years if `details_avail = TRUE`:
  #       -> "country xyz is observed at ..." where ... are all years available!

  name_data <- data
  data <- eval_tidy(parse_expr(data))

  data_info_preproc <- preprocess_data(data)
  dim_data     <- data_info_preproc[[1]]
  name_country <- data_info_preproc[[2]][[1]]
  len_country  <- data_info_preproc[[2]][[2]]
  unq_year     <- data_info_preproc[[3]][[1]]
  sum_unq_year <- sum(unlist(data_info_preproc[[3]][[2]]))

  cat("The dimensions of the",
      crayon::green("main dataset"),
      crayon::cyan(paste0("(", name_data, ")")),
      "are: ",
      "\n",
      dim_data[1], "rows and ",
      "\n",
      dim_data[2], "columns, ",
      "\n",
      "with ",
      len_country,
      "different countries and ",
      sum_unq_year,
      "country x year observations!",
      "\n", "\n")
  if (details_miss) {
    for (i in 1:len_country) {
      from_year <- min(unq_year[[i]])
      to_year   <- max(unq_year[[i]])
      seqs_year <- seq(from = from_year, to = to_year)
      miss_year <- !(seqs_year %in% unq_year[[i]])
      miss_year <- seqs_year[miss_year]
      if (identical(miss_year, integer(0))) {
        cat(name_country[i],
            "is observed from ",
            from_year,
            "to ",
            to_year,
            crayon::green("without missings!"),
            "\n", "\n")
      } else {
        cat(name_country[i],
            "is observed from ",
            from_year,
            "to ",
            to_year,
            crayon::red("with missings at: "),
            crayon::red(miss_year), "\n", "\n")
      }
    }
  }
  if (!is.null(data_compare)) {
    name_data_compare <- data_compare
    data_compare <- eval_tidy(parse_expr(data_compare))

    data_info_preproc_2 <- preprocess_data(data_compare)
    dim_data_2     <- data_info_preproc_2[[1]]
    name_country_2 <- data_info_preproc_2[[2]][[1]]
    len_country_2  <- data_info_preproc_2[[2]][[2]]
    unq_year_2     <- data_info_preproc_2[[3]][[1]]
    sum_unq_year_2 <- sum(unlist(data_info_preproc_2[[3]][[2]]))

    cat("The dimensions of the",
        crayon::red("dataset used for comparison"),
        crayon::cyan(paste0("(", name_data_compare, ")")),
        "are:",
        "\n",
        dim_data_2[1], "rows and ",
        "\n",
        dim_data_2[2], "columns, ",
        "\n",
        "with ",
        len_country_2,
        "different countries and",
        sum_unq_year_2,
        "country x year observations!",
        "\n", "\n")
    if (details_miss) {
      for (i in 1:len_country) {
        from_year_2 <- min(unq_year_2[[i]])
        to_year_2   <- max(unq_year_2[[i]])
        seqs_year_2 <- seq(from = from_year_2, to = to_year_2)
        miss_year_2 <- !(seqs_year_2 %in% unq_year_2[[i]])
        miss_year_2 <- seqs_year_2[miss_year_2]
        if (identical(miss_year_2, integer(0))) {
          cat(name_country_2[i],
              "is observed from ",
              from_year_2,
              "to ",
              to_year_2,
              crayon::green("without missings!"),
              "\n", "\n")
        } else {
          cat(name_country_2[i],
              "is observed from ",
              from_year_2,
              "to ",
              to_year_2,
              crayon::red("with missings at: "),
              crayon::red(miss_year_2), "\n", "\n")
        }
      }
    }
  }
  if (isTRUE(details_avail && !is.null(data_compare))) {
    cat("In more detail, for the ",
        crayon::green("main dataset "),
        "we observe the following countries and years:", "\n", "\n")
    for (i in 1:len_country) {
      cat(name_country[i], unq_year[[i]], "\n", "\n")
    }
    cat("For the",
        crayon::red("dataset used for comparison "),
        "we observe the following countries and years:", "\n", "\n")
    for (i in 1:len_country_2) {
      cat(name_country_2[i], unq_year_2[[i]], "\n", "\n")
    }
  }
  if (isTRUE(details_avail && is.null(data_compare))) {
    cat("In more detail, we observe the following countries and year:", "\n")
    for (i in 1:len_country) {
      cat(name_country[i], unq_year[[i]], "\n", "\n")
    }
  }
}
