

min_available <- 3
max_missings <- 3
ttl_available <- 9

old_vec <- c(rep(F, times = 2),
             rep(T, times = 4),
             rep(F, times = 2),
             rep(T, times = 2),
             rep(T, times = 1),
             rep(F, times = 2))
old_pattern <- rle(old_vec)

id_ones <- which(old_pattern[[2]])
# kick_id_ones <- old_pattern[[1]][id_ones] <= min_available
# keep_id_ones <- id_ones[!kick_id_ones]

id_zero <- which(!old_pattern[[2]])
kick_id_zero <- old_pattern[[1]][id_zero] >= max_missings
keep_id_zero <- id_zero[!kick_id_zero]

pattern_replace <- rep(FALSE, times = length(old_pattern[[2]]))
pattern_replace[c(id_ones, keep_id_zero)] <- TRUE
new_pattern <- old_pattern
new_pattern[[2]] <- pattern_replace
new_vec <- inverse.rle(new_pattern)



new_vec
View(data.frame(matrix(as.numeric(c(old_vec, new_vec)), nrow = 2, byrow = TRUE)))

# y_sel <- rle(y_bol)
#
# years_avail     <- y_seq[y_bol]
# years_not_avail <- y_seq[!y_bol]

# cat(sprintf(paste("For county:", ID[i], "\n", "Num available years:", )))

# filter dataset for all countries where the number of time periods where open
# is observed (non-missing) is at least equal to num_obs

# data_num_open <- data_raw %>% filter(!is.na(open)) %>%
#   select(country, year)
#
# count_open <- data_num_open %>%  group_by(country) %>% summarise(count = n())
#
# replace_var(data = data_raw,
#             var_name = "country",
#             entry_old = "Iran, Islamic Republic of",
#             entry_new = "Iran")

#
# string_format <- paste0(rep("%.3f", times = 5), collapse = " ")
# string_print1 <- paste0("init values:     ", string_format, "\n")
# string_print2 <- paste0("true values:     ", string_format, "\n")
# string_print3 <- paste0("mean values:     ", string_format, "\n")
#
# args_print1 <- c(list(fmt = string_print1), c(1:5))
# args_print2 <- c(list(fmt = string_print2), val_true)
# args_print3 <- c(list(fmt = string_print3), val_mean)
#
# cat(sprintf("##########################################################\n"))
# cat(do.call(sprintf, args = args_print1))
# cat(do.call(sprintf, args = args_print2))
# cat(do.call(sprintf, args = args_print3))
#
#
#
