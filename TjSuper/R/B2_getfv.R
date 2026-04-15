
getfv <- function(data) {
  # 输入检查：确保输入是数据框
  if (!inherits(data, c("data.frame", "tbl_df"))) {
    stop("输入必须是data.frame或tibble格式！")
  }

  # 初始化空列表存储结果
  all_attrs <- list()

  # 遍历每个变量，提取属性
  for (var_name in colnames(data)) {
    # 提取当前变量的所有属性
    var_attrs <- attributes(data[[var_name]])

    # 如果变量没有属性（var_attrs为NULL），则存储空列表
    if (is.null(var_attrs)) {
      all_attrs[[var_name]] <- list()
    } else {
      all_attrs[[var_name]] <- var_attrs
    }
  }

  return(all_attrs)
}
