#' 批量生成统一因子水平设置(Super_param生态的附属参数)
#'
#' 为多个变量创建具有相同因子水平（levels）和标签（labels）的设置列表，同时允许指定例外变量使用不同的因子配置，
#' 方便在数据预处理中快速批量定义因子变量属性，可直接用于需要因子水平设置参数的函数（如super_param的factor_levels参数）。
#'
#' @param levels 向量，必填参数。指定vars中所有变量共同的因子水平（顺序决定因子的排序），如c("低", "中", "高")或c(0, 1, 2)。
#' @param labels 向量，必填参数。与levels对应的因子标签，长度必须与levels一致，用于替换原始水平的显示名称，
#'   如levels=c(0,1)时，labels=c("否", "是")可将0显示为"否"、1显示为"是"。
#' @param exceptions 列表，可选参数。指定例外变量的因子设置，格式为`list(变量名=list(levels=..., labels=...))`。
#'   这些变量将使用自己的levels和labels，而非vars中定义的统一设置。例如`list("var4"=list(levels=c("A","B"), labels=c("类型A","类型B")))`。
#'
#' @returns 列表，其中每个元素对应一个变量的因子设置：
#'   - vars中指定的变量：每个元素为`list(levels=输入的levels, labels=输入的labels)`
#'   - exceptions中指定的变量：每个元素为`list(levels=例外变量的levels, labels=例外变量的labels)`
#'   列表元素名称为对应的变量名，可直接作为因子水平设置参数使用。
#'
#' @export
#'
#' @examples
#' # 示例1：基础批量因子设置（适用于多个变量共享相同水平和标签）
#' # 为多个二分类变量统一设置levels=c(0,1)和labels=c("no","yes")
#' basic_factors <- fast_factor_same(
#'   vars = c("smokev", "smoken", "drinkev", "drinkl"),  # 多个变量
#'   levels = c(0, 1),
#'   labels = c("no", "yes")
#' )
#'
#' # 示例2：包含例外变量的复杂因子设置
#' # 大部分变量用统一设置，少数变量用自定义水平和标签
#' complex_factors <- c(
#'   # 批量处理共享相同水平的变量（共40+个变量）
#'   fast_factor_same(
#'     vars = c(
#'       "smokev", "smoken", "drinkev", "drinkl", "joga", "walk1kma",
#'       "walk100a", "chaira", "climsa", "stoopa", "lifta", "dimea",
#'       "armsa", "phonea", "housewka", "mealsa", "shopa", "medsa",
#'       "moneya", "urina", "batha", "toilta", "beda", "eata", "dressa",
#'       "hlthlm_c", "hibpe", "diabe", "cancre", "lunge", "hearte",
#'       "stroke", "psyche", "arthre", "dyslipe", "livere", "kidneye",
#'       "digeste", "asthmae", "memrye", "rxhibp_c", "rxdiab_c",
#'       "cncrmeds_c", "cncrsurg", "cncrradn", "rxlung_c", "rxheart_c",
#'       "rxstrok_c", "rxpsych", "rxarthr_c", "rxdyslip_c", "rxliver_c",
#'       "rxkidney_c", "rxdigest_c", "rxmemry_c", "vgact_c", "mdact_c",
#'       "ltact_c"
#'     ),
#'     levels = c(0, 1),
#'     labels = c("no", "yes")
#'   ),
#'   # 例外变量：使用自定义水平和标签
#'   list(
#'     shlta = list(
#'       levels = c(1, 2, 3, 4, 5),
#'       labels = c("Very good", "Good", "Fair", "Poor", "Very Poor")
#'     ),
#'     drinkr_c = list(
#'       levels = c(0, 1, 2, 3, 4),
#'       labels = c("None", "Less than once per day", "Once per day",
#'                  "Twice per day", "More than twice per day")
#'     ),
#'     drinkn_c = list(
#'       levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
#'       labels = c("None", "Less than once per day", "Once a month",
#'                  "2 to 3 days a month", "Once a week", "2 to 3 days a week",
#'                  "4 to 6 days a week", "Daily", "Twice a day",
#'                  "More than twice a day")
#'     )
#'   )
#' )
#'
#' # 上述结果可直接用于需要因子水平设置的函数（如super_param）
#' # 示例：在数据预处理中应用因子设置
#' \dontrun{
#' # 假设df为原始数据集
#' preprocessed_data <- super_param(
#'   data = df,
#'   factor_levels = complex_factors  # 直接传入生成的因子设置列表
#' )
#' }
fast_factor_same <- function(vars, levels, labels, exceptions = NULL) {
  # 检查输入有效性
  if (!is.character(vars)) {
    warning("vars必须是字符向量（变量名）")
    return(NULL)
    }
  if (length(levels) != length(labels)) {
    warning("levels和labels长度必须一致")
    return(NULL)}

  # 批量生成共同变量的因子列表
  common_list <- setNames(
    lapply(vars, function(var) {
      list(levels = levels, labels = labels)
    }),
    vars  # 用变量名作为列表名称
  )

  # 如果有例外变量，合并到结果中（例外变量格式：list(变量名 = list(levels=..., labels=...))）
  if (!is.null(exceptions)) {
    if (!is.list(exceptions)) {
      warning("exceptions必须是列表，格式如list(变量名=list(levels=..., labels=...))")
      return(NULL)}
    common_list <- c(common_list, exceptions)
  }

  return(common_list)
}
