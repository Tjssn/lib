getfv <- function(data) {
  # 输入检查：确保输入是数据框
  if (!inherits(data, c("data.frame", "tbl_df", "tbl"))) {
    return(data)
    message("输入必须是 data.frame 或 tibble 格式！")
    message(
      "\033[34m", "data", "\033[0m",
      "must be the ",
      "\033[31m", 'class', "\033[0m",
      "  data.frame or tibble , not ",
      "\033[31m", class(data), "\033[0m"
    )
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
      all_attrs[[var_name]] <- var_attrs%>%
        as.data.frame()
    }
  }
  all_attrs_data <- all_attrs%>%
    # 1. 过滤：只保留数据框类型的元素
    purrr::keep(is.data.frame) %>%
    # 2. 关键步骤：统一列类型（factor_levels/factor_labels 转为字符型）
    purrr::map(~ {
      # 对存在的列进行类型转换（避免不存在列报错）
      if ("factor_levels" %in% colnames(.x)) {
        .x$factor_levels <- as.character(.x$factor_levels)  # 统一转为字符型
      }
      if ("factor_labels" %in% colnames(.x)) {
        .x$factor_labels <- as.character(.x$factor_labels)  # 统一转为字符型
      }
      if ("levels" %in% colnames(.x)) {  # 从数据看部分表有"levels"列，也统一类型
        .x$levels <- as.character(.x$levels)
      }
      return(.x)
    }) %>%
    # 3. 合并数据框并添加变量名
    purrr::imap_dfr(~ .x %>% mutate(var_name = .y)) %>%
    # 4. 调整列顺序
    dplyr::select(var_name, everything())%>%
    dplyr::select(-levels,-class)

  return(all_attrs_data)
}
