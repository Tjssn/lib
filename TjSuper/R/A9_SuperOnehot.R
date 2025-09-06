#' 对分类变量进行独热编码
#'
#' 该函数用于将数据框中指定的分类变量转换为独热编码（One-Hot Encoding）形式，
#' 为每个类别创建一个二进制指示变量，便于机器学习模型或统计分析使用。
#'
#' @param data 数据框(data.frame)或tibble，必填参数。包含需要进行独热编码的数据集。
#' @param var_names 字符向量，必填参数。指定需要进行独热编码的变量名称。
#' @param sep 字符型，可选参数，默认值为"_"。新生成变量名的分隔符，用于连接原变量名和类别值，
#'   如变量"gender"的类别"male"会生成"gender_male"。
#'
#' @return 处理后的数据集，其中指定的分类变量被替换为对应的独热编码变量。
#'
#' @details
#' 函数通过以下步骤实现独热编码：
#' 1. 对每个指定的变量，提取其所有唯一类别；
#' 2. 为每个类别创建一个新变量，当原变量取值为该类别时新变量为1，否则为0；
#' 3. 新变量名由原变量名、分隔符和类别值组成；
#' 4. 移除原始的分类变量，仅保留新生成的独热编码变量。
#'
#' @note
#' 建议在使用前确保变量确实为分类变量（因子型或字符型）；
#' 若变量包含大量唯一值（高基数），独热编码可能导致维度爆炸，需谨慎使用；
#' 函数会自动处理缺失值（NA），将其作为一个独立类别进行编码。
#'
#' @examples
#' # 创建示例数据
#' df <- data.frame(
#'   gender = c("男", "女", "男", "未知", NA),
#'   education = c("高中", "本科", "本科", "硕士", "高中"),
#'   age = c(25, 30, 35, 40, 45)  # 数值型变量不会被处理
#' )
#'
#' # 对性别和教育程度进行独热编码
#' df_encoded <- SuperOnehot(
#'   data = df,
#'   var_names = c("gender", "education"),
#'   sep = "_"
#' )
#'
#' # 查看结果
#' df_encoded
#'
#' @export
SuperOnehot <- function(data, var_names,sep="_") {
  for (var in var_names) {
    categories <- unique(data[[var]])
    for (cat in categories) {
      new_col <- paste0(var, sep, cat)
      data[[new_col]] <- as.integer(data[[var]] == cat)
    }
    data[[var]] <- NULL
  }
  return(data)
}
