normalize_column_names_ultimate <- function(data, log_print_len = 30) {
  if (!inherits(data, "data.frame")) {
    stop("输入数据必须为数据框")
  }
  old_names <- names(data)
  X <- data.frame(old = old_names,
                  stringsAsFactors = FALSE,
                  check.names = FALSE)
  get_universal_name <- function(char) {
    char_list <- strsplit(char, "")[[1]]
    processed <- sapply(char_list, function(c) {
      if (grepl("[a-zA-Z0-9_]", c)) {

      } else {
        paste0("u", as.hexmode(utf8ToInt(c)))
      }
    })
    temp <- paste(processed, collapse = "")
    if (grepl("^[0-9]", temp)) {
      temp <- paste0("v", temp)
    }
    if (nchar(temp) == 0) {
      temp <- paste0("var_", which(old_names == char))
    }

    temp
  }
  legal_chars <- "a-zA-Z0-9_\u4e00-\u9fa5"
  X$Notallow <- as.numeric(sapply(X$old, function(name) {
    any(nchar(gsub(
      paste0("[", legal_chars, "]"), "", name
    )) > 0)
  }))
  X$Xnew <- sapply(X$old, function(name) {
    new_name <- gsub(paste0("[^", legal_chars, "]"), "_", name)
    gsub("_+", "_", new_name)
  })
  X$universal_name <- sapply(X$old, get_universal_name)
  X$Xnew <- make.unique(X$Xnew, sep = "_")
  X$Xnew <- gsub("_+", "_", X$Xnew)
  X$universal_name <- make.unique(X$universal_name, sep = "_")
  dfnew <- data
  colnames(dfnew) <- X$Xnew

  if (sum(X$Notallow) > 0) {
    relog <- X[X$Notallow == 1, c("old", "Xnew")]
    colnames(relog) <- c("Oldname", "Newname")
    relog$Oldname <- substr(relog$Oldname, 1, log_print_len)
    relog$Newname <- substr(relog$Newname, 1, log_print_len)

    tempsuccess <- paste0(nrow(relog), "个非法变量名被成功转化")
    cat("成功：", tempsuccess, "\n\n")

    cat("变量重命名对照表（截取前", log_print_len, "位）\n", sep = "")
    max_old <- max(nchar(relog$Oldname), nchar("Oldname"))
    max_new <- max(nchar(relog$Newname), nchar("Newname"))
    cat(sprintf(
      paste0("%-", max_old, "s  %-", max_new, "s\n"),
      "Oldname",
      "Newname"
    ))
    cat(sprintf(
      paste0("%-", max_old, "s  %-", max_new, "s\n"),
      strrep("-", max_old),
      strrep("-", max_new)
    ))
    for (i in 1:nrow(relog)) {
      cat(sprintf(
        paste0("%-", max_old, "s  %-", max_new, "s\n"),
        relog$Oldname[i],
        relog$Newname[i]
      ))
    }
    cat("\n")
  } else {
    cat("信息：所有变量名均为规则变量名，跳过转化步骤\n\n")
  }
  X <- X[, c("old", "Xnew", "universal_name")]
  colnames(X) <- c("original", "rule", "unicode")

  return(list(data = dfnew, variable_mapping = X))
}
