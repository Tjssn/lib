Tj_add_columns <- function(data, add = NULL) {
  if (!is.data.frame(data)) {
    warning("输入必须是数据框（data.frame)")
    return(NULL)
  }
  if (is.null(add) || (is.list(add) && length(add) == 0)) {
    return(data)
  }
  if (!is.list(add)) {
    warning("add必须是列表，格式如list(新列名 = 表达式)")
    return(NULL)
  }
  result <- data
  for (col_name in names(add)) {
    if (is.null(col_name) || col_name == "") {
      warning("存在未命名的列，已跳过")
      next
    }
    expr <- add[[col_name]]
    tryCatch({
      if (is.character(expr) && length(expr) == 1) {
        new_col <- eval(parse(text = expr), envir = result)
      } else {
        new_col <- expr
      }
      if (length(new_col) != 1 && length(new_col) != nrow(result)) {
        warning(paste("列", col_name, "长度必须为1或与数据行数一致，已跳过"))
        next
      }
      result[[col_name]] <- new_col

    }, error = function(e) {
      warning(paste("列", col_name, "add参数执行出错：", e$message, "，已跳过"))
    })
  }

  return(result)
}
