Tj_filter_rows <- function(data, condition = NULL) {
  original_data <- data
  if (!is.data.frame(data)) {
    message("错误：输入必须是数据框（data.frame），将返回原始输入")
    return(original_data)
  }
  if (is.null(condition) || (is.character(condition) && condition == "")) {
    message("未提供筛选条件，返回原始数据")
    return(data)
  }
  if (!is.expression(condition) && !is.character(condition)) {
    message("错误：筛选条件（condition）必须是表达式或字符串，例如：condition = ~age > 18 或 condition = 'age > 18 & sex == 'male''，将返回原始数据")
    return(original_data)
  }
  if (is.character(condition)) {
    tryCatch({
      condition_expr <- parse(text = condition)
    }, error = function(e) {
      message("错误：条件字符串解析失败：", e$message, "，将返回原始数据")
      return(original_data)
    })
  } else {
    condition_expr <- condition
  }

  tryCatch({

    filter_idx <- eval(condition_expr, envir = data)
    if (!is.logical(filter_idx)) {
      message("错误：筛选条件必须返回逻辑向量（TRUE/FALSE），将返回原始数据")
      return(original_data)
    }
    if (length(filter_idx) != nrow(data)) {
      message("错误：筛选条件返回的逻辑向量长度（", length(filter_idx), "）与数据行数（", nrow(data), "）不匹配，将返回原始数据")
      return(original_data)
    }
    filter_idx[is.na(filter_idx)] <- FALSE
    result <- data[filter_idx, , drop = FALSE]
    message("筛选完成，保留 ", nrow(result), " 行（原始 ", nrow(data), " 行）")
    return(result)
  }, error = function(e) {
    message("错误：筛选执行失败：", e$message, "，将返回原始数据")
    return(original_data)
  })
}
