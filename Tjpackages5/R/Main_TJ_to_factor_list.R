#' 将因子信息数据框转换为start与标签的列表
#'
#' 该函数用于处理包含变量因子信息的数据框，提取每个变量对应的start及其标签，
#' 并将结果组织为列表形式，方便后续将变量转换为带标签的因子时使用。
#'
#' @param factor_df 数据框（data.frame），必须包含三列：
#'   - "最终变量名"：字符串类型，存储变量的名称（如示例中的"diffweek"）
#'   - "start"：存储因子的原始水平值（如示例中的0、1、2）
#'   - "因子标签"：字符串类型，存储对应start的标签（如示例中的"第0周"、"第1周"、"第2周"）
#'
#' @returns 一个列表，列表的每个元素对应"最终变量名"中的一个唯一变量。每个元素本身也是一个列表，包含两个向量：
#'   - levels：对应变量的start向量（取自"start"列）
#'   - labels：对应变量的因子标签向量（取自"因子标签"列），与levels顺序一一对应
#'
#' @export
#'
#' @examples
#' # 创建示例数据框
#' factor_info <- data.frame(
#'   最终变量名 = c("diffweek", "diffweek", "diffweek"),
#'   start = c(0, 1, 2),
#'   因子标签 = c("第0周", "第1周", "第2周"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 调用函数
#' factor_list <- Tj_factor_list(factor_info)
#'
#' # 查看结果
#' print(factor_list)
#' # 输出：
#' # $diffweek
#' # $diffweek$levels
#' # [1] 0 1 2
#' #
#' # $diffweek$labels
#' # [1] "第0周" "第1周" "第2周"
Tj_factor_list <- function(factor_df,search_name=T) {
  required_cols <- c("最终变量名", "start", "label")
  if(!isTRUE( search_name)){  colnames(factor_df) <- required_cols}
  if (!all(required_cols %in% colnames(factor_df))) {
    warning("数据集必须包含以下列：", paste(required_cols, collapse = "、"))
    return(NULL)
  }

  vars <- unique(factor_df$最终变量名)
  if (length(vars) == 0) {
    warning("数据集中没有有效的最终变量名")
    return(NULL)
  }

  factor_list <- list()

  for (var in vars) {
    sub_df <- factor_df[factor_df$最终变量名 == var, ]

    levels <- sub_df$start
    labels <- sub_df$label

    if (length(levels) != length(labels)) {
      warning("变量'", var, "'的start与标签数量不匹配，已跳过")
      next
    }

    factor_list[[var]] <- list(levels = levels, labels = labels)
  }

  return(factor_list)
}
