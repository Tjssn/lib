#' 基于fv汇总数据筛选变量
#'
#' 该函数用于根据指定条件筛选由getfv()生成的汇总数据，并可返回原始数据中符合条件的变量子集，
#' 或直接提取汇总数据中的指定列信息，方便快速筛选和提取经过fv()处理的变量。
#'
#' @param data 数据框(data.frame)或tibble，可选参数。原始数据集，仅当getvar='data.frame'且未提供fv_data时必需，
#'   用于生成汇总数据或提取符合条件的变量子集。
#' @param condition 字符型，必填参数。筛选条件，基于getfv汇总数据的列构建的逻辑表达式字符串，
#'   如'domain == "demographic"'、'reclass == "factor"'或'label %like% "年龄"'。
#' @param getvar 字符型，可选参数。指定返回结果的类型，默认为"var_name"。
#'   若为"data.frame"，返回原始数据中符合条件的变量子集；
#'   若为汇总数据中的列名（如"var_name"、"label"），则返回该列的去重向量。
#' @param fv_data 数据框(data.frame)，可选参数。由getfv()生成的汇总数据，若已提前计算可直接传入以提高效率，
#'   若未提供则自动基于data生成。
#'
#' @return
#' 若getvar="data.frame"，返回原始数据中符合条件的变量子集（数据框）；
#' 若getvar为汇总数据的列名，返回该列的去重向量；
#' 若无符合条件的记录或有效变量，返回NULL并提示信息。
#'
#' @section 重要提示:
#' \strong{⚠️} condition必须是单个字符串形式的逻辑表达式，且只能使用getfv汇总数据中存在的列名；
#' 当getvar="data.frame"时，会自动检查变量在原始数据中的存在性，忽略不存在的变量；
#' 若提前生成了getfv汇总数据，建议通过fv_data参数传入，避免重复计算。
#'
#' @examples
#' # 准备示例数据
#' df <- data.frame(
#'   age = c(25, 30, 35, NA, 40),
#'   gender = c(1, 2, 1, 2, 1),
#'   score = c("85", "90", "NA", "75", "80")
#' )
#'
#' # 先批量处理变量并生成汇总数据
#' df_processed <- fv_all(df, domain = c("demographic", "demographic", "academic"))
#' fv_summary <- getfv(df_processed)  # 假设getfv()可生成包含var_name和domain的汇总数据
#'
#' # 示例1：返回领域为demographic的变量子集
#' demo_vars <- filterfv(
#'   data = df_processed,
#'   condition = 'domain == "demographic"',
#'   getvar = "data.frame"
#' )
#' head(demo_vars)
#'
#' # 示例2：提取因子型变量的名称（假设汇总数据有reclass列）
#' factor_vars <- filterfv(
#'   fv_data = fv_summary,
#'   condition = 'reclass == "factor"',
#'   getvar = "var_name"
#' )
#' factor_vars
#'
#' # 示例3：直接使用条件筛选并返回标签信息
#' age_labels <- filterfv(
#'   data = df_processed,
#'   condition = 'var_name == "age"',
#'   getvar = "label"
#' )
#' age_labels
#'
#' @export

filterfv <- function(
    data,
    condition,
    getvar = "var_name",
    fv_data = NULL
) {
  # 加载依赖包
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请安装dplyr包：install.packages('dplyr')")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("请安装rlang包：install.packages('rlang')")
  }

  # 输入验证
  if (missing(condition) || !is.character(condition) || length(condition) != 1) {
    stop("condition必须是单个字符串（如'domain == \"Model\"'）")
  }

  # 生成或验证汇总数据（getfv的结果，核心数据来源）
  if (is.null(fv_data)) {
    fv_data <- getfv(data)  # 自动生成汇总数据
    message("已自动生成getfv汇总数据")
  } else {
    if (!inherits(fv_data, "data.frame")) {
      stop("fv_data必须是getfv生成的汇总数据（data.frame格式）")
    }
    if (!"var_name" %in% colnames(fv_data)) {
      stop("fv_data必须包含'var_name'列（getfv处理后的标准列）")
    }
  }

  # 筛选汇总数据（基于条件）
  condition_expr <- rlang::parse_expr(condition)
  filtered_fv <- fv_data %>%
    dplyr::filter(!!condition_expr) %>%  # 仅筛选汇总数据
    dplyr::distinct()  # 汇总数据去重

  # 无筛选结果时返回NULL
  if (nrow(filtered_fv) == 0) {
    message("汇总数据中无符合条件的记录")
    return(NULL)
  }

  # 处理返回结果
  if (getvar == "data.frame") {
    # 情况1：返回原始数据中符合条件的变量子集
    # 从汇总数据筛选结果中提取变量名
    target_vars <- filtered_fv$var_name
    # 检查变量在原始数据中的存在性
    valid_vars <- intersect(target_vars, colnames(data))
    invalid_vars <- setdiff(target_vars, colnames(data))

    if (length(invalid_vars) > 0) {
      message("原始数据中不存在以下变量，已忽略：", paste(invalid_vars, collapse = ", "))
    }
    if (length(valid_vars) == 0) {
      message("无有效变量可从原始数据中提取")
      return(NULL)
    }
    return(data %>% dplyr::select(dplyr::all_of(valid_vars)))

  } else if (getvar %in% colnames(filtered_fv)) {
    # 情况2：严格提取汇总数据中的指定列，返回去重向量
    # 直接从筛选后的汇总数据中提取列，不涉及原始数据
    result_vector <- filtered_fv %>%
      dplyr::pull({{ getvar }}) %>%  # 提取汇总数据的目标列
      unique()  # 确保向量元素唯一
    return(result_vector)

  } else {
    # 无效的getvar参数
    stop("getvar必须是'data.frame'或汇总数据中的列名，当前汇总数据列名：\n",
         paste(colnames(filtered_fv), collapse = ", "))
  }
}

