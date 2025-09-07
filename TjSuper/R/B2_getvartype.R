
TJ_get_vartype <- function(x, max_level =  max_level, max_level_pos = max_level_pos) {

  max_level <- min(max_level, 12)
  max_level <- max(max_level, 1)
  max_level_pos <- max(max_level_pos, 1)


  base_type <- typeof(x)
  inherit_types <- class(x)
  special_type <- character(0)


  if (is.null(x)) {
    special_type <- "NULL(无值)"
  } else if (is.function(x)) {
    special_type <- "函数(function)"
  } else if (is.environment(x)) {
    special_type <- "环境(environment)"
  }

  if (!is.null(x) && !is.function(x) && !is.environment(x)) {
    if (inherits(x, "matrix")) special_type <- c(special_type, "矩阵(matrix)")
    else if (inherits(x, "array") && !inherits(x, "matrix")) special_type <- c(special_type, "数组(array)")
    if (inherits(x, "data.frame")) special_type <- c(special_type, ifelse(inherits(x, "tbl_df"), "tibble(tbl_df)", "数据框(data.frame)"))
    else if (is.list(x)) special_type <- c(special_type, "列表(list)")
  }

  if (inherits(x, "Date")) special_type <- c(special_type, "日期型(Date)")
  else if (inherits(x, c("POSIXct", "POSIXlt"))) special_type <- c(special_type, "datetime型(POSIX)")
  else if (inherits(x, "difftime")) special_type <- c(special_type, "时间差(difftime)")
  else if (inherits(x, "ts")) special_type <- c(special_type, "时间序列(ts)")

  if (inherits(x, "factor")) special_type <- c(special_type, ifelse(inherits(x, "ordered"), "有序因子(ordered factor)", "因子(factor)"))
  if (base_type == "logical") special_type <- c(special_type, "逻辑型(logical)")
  if (length(special_type) == 0) special_type <- "无特殊类型"


  var_values <- NULL


  if (inherits(x, c("factor", "ordered", "character"))) {
    if (inherits(x, c("factor", "ordered"))) {
      values_all <- levels(x)
    } else {
      values_all <- unique(x[!is.na(x)])
    }

    if (length(values_all) == 0) {
      var_values <- "无有效水平(全为NA)"
    } else {
      values_truncated <- substr(values_all, 1, max_level_pos)
      if (length(values_truncated) > max_level) {
        values_show <- c(values_truncated[1:max_level], paste0("...(共", length(values_truncated), "个)"))
      } else {
        values_show <- values_truncated
      }
      var_values <- paste(values_show, collapse = "/")
    }


  } else if (is.numeric(x)) {
    mean_val <- mean(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)

    if (is.na(mean_val)) {
      var_values <- "定量变量(全为NA)"
    } else {

      var_values <- sprintf("%.1f(%.1f,%.1f)", mean_val, max_val, min_val)
    }


  } else {
    var_values <- "非定性/定量变量(如日期、逻辑型等)"
  }


  list(
    基础类型 = base_type,
    继承类型 = inherit_types,
    特殊类型标识 = special_type,
    变量值展示 = var_values
  )
}

Tj_rename_duplicate_col <- function(data, log) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("检测到重复列名需要'stringr'包，请先安装：install.packages('stringr')")
  }
  original_names <- names(data)
  new_names <- original_names
  r_renamed_pattern <- "^(.*?)\\.\\.(\\d+)$"


  r_renamed_indices <- grep(r_renamed_pattern, original_names)
  if (length(r_renamed_indices) > 0) {
    r_renamed_cols <- original_names[r_renamed_indices]
    match_results <- stringr::str_match(r_renamed_cols, r_renamed_pattern)
    base_names <- match_results[, 2]
    number_suffix <- as.numeric(match_results[, 3])


    na_indices <- which(is.na(base_names))
    if (length(na_indices) > 0) {
      for (i in na_indices) {
        base_names[i] <- r_renamed_cols[i]
        number_suffix[i] <- 1
        log <- c(log, paste("警告：列名", r_renamed_cols[i], "不匹配R重命名模式，使用原始名称作为基础"))
      }
    }


    for (i in seq_along(r_renamed_indices)) {
      base_name <- base_names[i]
      suffix <- number_suffix[i]
      new_col <- paste0(base_name, "_", suffix)
      while (new_col %in% new_names) {
        suffix <- suffix + 1
        new_col <- paste0(base_name, "_", suffix)
      }
      new_names[r_renamed_indices[i]] <- new_col
    }


    renamed_pairs <- data.frame(
      原始列名 = r_renamed_cols,
      新列名 = new_names[r_renamed_indices],
      check.names = FALSE
    )
    log <- c(log, "检测到R自动生成的重复列名，已重命名：")
    log <- c(log, paste(capture.output(print(renamed_pairs)), collapse = "\n"))
  }


  name_counts <- table(new_names)
  duplicate_names <- names(name_counts)[name_counts > 1]
  if (length(duplicate_names) > 0) {
    unique_names <- make.unique(new_names)
    changed_indices <- which(new_names != unique_names)
    if (length(changed_indices) > 0) {
      changed_pairs <- data.frame(
        原始列名 = new_names[changed_indices],
        新列名 = unique_names[changed_indices],
        check.names = FALSE
      )
      log <- c(log, "检测到非R自动生成的重复列名，已生成唯一名称：")
      log <- c(log, paste(capture.output(print(changed_pairs)), collapse = "\n"))
      new_names <- unique_names
    }
  }


  if (!all(new_names == original_names)) {
    names(data) <- new_names
    n_changed <- sum(new_names != original_n ames)
    log <- c(log, paste("列名重命名完成，共修改", n_changed, "列"))
  } else {
    log <- c(log, "未检测到需要重命名的重复列名")
  }

  list(data = data, log = log)
}
