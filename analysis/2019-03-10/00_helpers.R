replace_var <- function(data, var_name, entry_old, entry_new) {
  data_col <- paste("data", "$", var_name, sep = "")
  data_col[data_col == entry_old] <- entry_new
}
generate_countries_num_var <- function(data, var, num) {
  data_num_open <- data %>% filter(!is.na(eval(parse(text = var)))) %>%
    select(country, year)

  count_open <- data_num_open %>%  group_by(country) %>% summarise(count = n())

  countries_num_open <- count_open$country[count_open$count >= num]
  countries_num_open
}
generate_pattern_row <- function(data, y_all) {
  y     <- data$year
  y_bol <- y_all %in% y
  y_bol <- as.numeric(y_bol)
  return(matrix(y_bol, nrow = 1, byrow = TRUE))
}
change_pattern_matrix <- function(pattern_matrix_old,
                                  min_available,
                                  max_missings) {
  pattern_matrix_new <- matrix(nrow = nrow(pattern_matrix_old),
                               ncol = ncol(pattern_matirx_old))
  len_pattern <- nrow(pattern_matrix_old)

  for (i in 1:len_pattern) {
    pattern_old <- rle(pattern_matrix_old[i, ])

    id_ones <- which(pattern_old[[2]])
    id_zero <- which(!pattern_old[[2]])

    kick_id_zero <- pattern_old[[1]][id_zero] >= max_missings
    keep_id_zero <- id_zero[!kick_id_zero]

    pattern_repl <- rep(FALSE, times = length(pattern_old[[2]]))
    pattern_repl[c(id_ones, keep_id_zero)] <- TRUE
    pattern_new <- pattern_old
    pattern_new[[2]] <- pattern_repl
    pattern_matrix_new[i, ] <- inverse.rle(pattern_new)
  }
  return(pattern_matrix_new)
}
analyse_pattern_variables <- function(data,
                                      var_list,
                                      country_list = "all",
                                      fixing = "country") {
  # function analyses a given variable for availability of consecutive years
  # naming: year -> y e.g. min_y_ttl mean smalles year taken over all
  # naming: total -> ttl, consecutive -> cnsctv
  #
  #
  #
  sub_data_1 <- select(data, country, year, var_list)
  min_y_ttl <- sub_data_1 %>% group_by(country) %>%
    summarise(min_y = min(year))
  min_y_ttl <- min(min_y_ttl$min_y)
  max_y_ttl <- sub_data_1 %>% group_by(country) %>%
    summarise(max_y = max(year))
  max_y_ttl <- max(max_y_ttl$max_y)

  y_seq <- seq(from = min_y_ttl, to = max_y_ttl, by = 1)

  if (country_list == "all") {
    ID <- unique(data$country)
  } else {
    ID <- country_list
  }
  len_ID  <- length(ID)
  len_var <- length(var_list)

  if (fixing == "variable") {
    results <- rep(list(data.frame()), times = len_var)
    names(results) <- paste("pattern", var_list, sep = "_")
    for (i in 1:len_var) {
      out <- matrix(nrow = 0, ncol = length(y_seq))
      for (j in 1:len_ID) {
        sub_dat_2 <- filter(sub_data_1, country == ID[j], !is.na(var_list[i]))
        pattern   <- generate_pattern_row(data = sub_dat_2, y_all = y_seq)
        out <- rbind(out, pattern)
      }
      out <- data.frame(out)
      colnames(out) <- c(as.character(y_seq))
      out <- cbind(country = ID, out)
      results[[i]] <- out
    }
    return(results)
  } else if (fixing == "country") {
    results <- rep(list(data.frame()), times = len_ID)
    names(results) <- paste("pattern", ID, sep = "_")
    for (i in 1:len_ID) {
      out <- matrix(nrow = 0, ncol = length(y_seq))
      for (j in 1:len_var) {
        sub_dat_2 <- filter(sub_data_1, country == ID[i], !is.na(var_list[j]))
        pattern   <- generate_pattern_row(data = sub_dat_2, y_all = y_seq)
        out <- rbind(out, pattern)
      }
      out <- data.frame(out)
      colnames(out) <- c(as.character(y_seq))
      out <- cbind(variable = var_list, out)
      results[[i]] <- out
    }
  } else {
    stop("Error: invalid fixing variable! Choose 'country' or 'variable'!")
  }
  return(results)
}
generate_pattern_variables <- function(data,
                                       var_list,
                                       country_list = "all") {
  # function analyses a given variable for availability of consecutive years
  # naming: year -> y e.g. min_y_ttl mean smalles year taken over all
  # naming: total -> ttl, consecutive -> cnsctv
  #
  #
  #
  sub_data_1 <- select(data, country, year, var_list)
  min_y_ttl <- sub_data_1 %>% group_by(country) %>%
    summarise(min_y = min(year))
  min_y_ttl <- min(min_y_ttl$min_y)
  max_y_ttl <- sub_data_1 %>% group_by(country) %>%
    summarise(max_y = max(year))
  max_y_ttl <- max(max_y_ttl$max_y)

  y_seq <- seq(from = min_y_ttl, to = max_y_ttl, by = 1)

  if (country_list == "all") {
    ID <- unique(data$country)
  } else {
    ID <- country_list
  }
  len_ID  <- length(ID)
  len_var <- length(var_list)

  valid_country <- logical(len_ID)
  for (i in 1:len_ID) {
    p_mat_old <- matrix(nrow = 0, ncol = length(y_seq))
    for (j in 1:len_var) {
      sub_dat_2 <- filter(sub_data_1, country == ID[i], !is.na(var_list[j]))
      pattern   <- generate_pattern_row(data = sub_dat_2, y_all = y_seq)
      p_mat_old <- rbind(p_mat_old, pattern)
    }
    p_mat_new <- change_pattern_matrix(pattern_matrix_old = p_mat_old,
                                       min_available = 10,
                                       max_missings = 2)
    results[[i]] <- p_mat_new

    pattern_check <- colSums(p_mat_new)
    pattern_check %in% c(0, len_var)
    valid_country[i] <- identical(sum(!pattern_check), 0)
  }

  results <- rep(list(data.frame()), times = len_ID)
  names(results) <- paste("pattern", ID, sep = "_")
  return(list(all_pattern = results,
              valid_countries = ID[valid_country],
              invalid_countries = ID[!valid_country]))
}

  #   if (max(y_sel$lengths[y_sel$values]) >= min_years) {
  #     y_sel <- with(rle(y_bol),
  #                   rep(lengths == max(lengths[values]) & values , lengths))
  #     y_taken <- y_seq[y_sel]
  #     y_ID    <- years %in% y_taken
  #   } else {
  #     y_ID <- rep(FALSE, times = length(years))
  #   }
  #   y_consec <- c(y_consec, y_ID)
  # }
  # data <- cbind(data, y_consec)
  # data <- filter(data, y_consec == TRUE)
  # return(data)
