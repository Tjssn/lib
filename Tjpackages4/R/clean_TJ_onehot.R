TJ_onehot <- function(data, var_names) {
  for (var in var_names) {
    categories <- unique(data[[var]])
    for (cat in categories) {
      new_col <- paste0(var, "_", cat)
      data[[new_col]] <- as.integer(data[[var]] == cat)
    }
    data[[var]] <- NULL
  }
  return(data)
}

