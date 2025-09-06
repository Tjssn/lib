#' 对数据框所有变量批量应用fv()函数
#'
#' 该函数用于对数据框中的所有变量批量应用fv()函数，实现全数据集的变量属性标准化处理。
#' 支持传递额外参数给fv()函数，保持与单个变量处理相同的参数优先级规则。
#'
#' @param data 数据框(data.frame)或tibble，必填参数。待处理的数据集，包含需要批量处理的变量。
#' @param ... 额外参数，将传递给fv()函数，如label、domain、reclass等。
#'
#' @return
#' 处理后的数据集，其中每个变量都经过fv()函数处理并添加了标准化属性。
#'
#' @section 重要提示:
#' \strong{⚠️} 输入数据必须是data.frame或tibble格式，否则将返回原始输入并提示错误；
#' 传递给...的参数将应用于所有变量，如需对不同变量设置不同参数，请单独使用fv()函数处理。
#'
#' @examples
#' # 准备示例数据
#' df <- data.frame(
#'   age = c(25, 30, 35, NA, 40),
#'   gender = c(1, 2, 1, 2, 1),
#'   score = c("85", "90", "NA", "75", "80")
#' )
#'
#' # 批量处理所有变量
#' df_processed <- fv_all(df)
#'
#' # 查看处理后变量的属性
#' attributes(df_processed$age)
#' attributes(df_processed$gender)
#'
#' # 传递额外参数（为所有变量设置相同的领域）
#' df_processed2 <- fv_all(df, domain = "demographic")
#' attributes(df_processed2$age)$domain  # 查看领域属性
#'
#' @export
fv_all <- function(data, ...) {
  # 输入检查：确保输入为数据框
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

  # 核心逻辑：对所有列应用fv()，支持传递额外参数（...）
  data %>%
    mutate(
      across(everything(), ~ fv(.x, ...))
    )
}
