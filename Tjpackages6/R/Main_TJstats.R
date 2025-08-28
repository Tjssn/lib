#' TjssnStat
#'
#'
#' @param Stat 字符型，指定分析模型类型。常用值包括"Summary"（默认，描述性统计与组间比较）、
#'   "COX.univariate"（单因素COX回归）、"LOGISTIC"（逻辑回归）、"LINEAR"（线性回归）等，决定分析的核心逻辑。
#' @param Object 字符型，指定分析结果在全局环境中的存储名称，后续可通过该名称调用结果。默认值为"OA"，
#'   建议根据分析内容自定义（如"sepsis_summary"）。
#' @param data_param 列表，配置输入数据信息。支持两种数据来源：
#'   - \code{super_param}：传入经\code{super_param}函数处理后的结果对象（如示例中的PAMR），自动读取标准化数据；
#'   - \code{data}：直接传入原始数据框（如df），适用于未经过\code{super_param}处理的场景。
#'   两者需指定其一，若同时存在，优先使用\code{super_param}。
#' @param var_param 列表，配置分析变量的筛选与分组规则：
#'   - \code{filter_super}：字符型，基于\code{super_param}结果的变量标签筛选条件（如"label %in% c('年龄','性别')"）；
#'   - \code{exclude_var}：字符向量，指定需排除的变量名称（如c("id","无关变量")）；
#'   - \code{include_var}：字符向量，指定需强制纳入的变量名称，优先级高于筛选条件；
#'   - \code{Group_by}：字符型，指定分组变量名称（如示例中的"脓毒症"），用于组间比较分析。
#' @param model_param 列表，配置分析模型的核心参数（以"Summary"类型为例）：
#'   - \code{normality_test_vars}：字符向量，指定需进行正态性检验的变量（如c("身高","体重")）；
#'   - \code{assume_normal_vars}：字符向量，直接假设为正态分布的变量（如示例中的c("diffday","APACHEII")），不执行检验；
#'   - \code{homogeneity_test_vars}：字符向量，指定需进行方差齐性检验的变量；
#'   - \code{assume_homogeneous_vars}：字符向量，直接假设方差齐性的变量，不执行检验；
#'   - \code{perform_comparison}：逻辑型，是否执行组间差异性检验（如t检验、卡方检验），默认TRUE；
#'   - \code{perform_description}：逻辑型，是否生成描述性统计量（如均数、中位数、百分比），默认TRUE；
#'   - \code{perform_pairwise}：逻辑型，是否执行多组间的两两比较（如ANOVA后的多重比较），默认TRUE；
#'   - \code{reference_group}：字符型，指定分组比较的参照组（如"对照组"），默认使用第一个水平。
#' @param output_param 列表，配置结果的展示格式：
#'   - \code{summary_display}：逻辑型，是否在控制台显示汇总表格，默认TRUE；
#'   - \code{percent_type}：字符型，指定百分比计算方式，可选"ROW"（行百分比）、"COL"（列百分比）、"TOTAL"（总百分比），示例中为"ROW"；
#'   - \code{digits_percent}：整数，百分比结果保留的小数位数（示例中为4）；
#'   - \code{digits_mean}：整数，均数保留的小数位数（默认NULL，自动判断）；
#'   - \code{digits_statistic}：整数，检验统计量（如t值、χ²值）保留的小数位数；
#'   - \code{digits_pvalue}：整数，P值保留的小数位数（示例中为5）；
#'   - \code{digits_odds_ratio}：整数，比值比（OR）等效应量保留的小数位数。
#' @param pack_param 列表，内部使用参数，用于配置依赖包的加载与版本控制，一般无需用户手动设置。
#' @param logprint 逻辑型，是否在控制台打印分析日志（包含步骤、警告、结果摘要），默认TRUE。
#' @param .call 内部参数，标记函数调用来源，用户无需设置。
#'
#' @return 无显式返回值。分析结果将保存到全局环境中以\code{Object}命名的列表对象中，包含：
#'   - \code{result.list}：核心结果，包括描述性统计表格、检验结果、P值等；
#'   - \code{log}：详细分析日志，记录变量筛选、检验执行、结果计算等过程；
#'   - \code{plots}：可视化结果（如箱线图、柱状图，若output_param中开启）。
#'
#' @examples
#' \dontrun{
#' # 示例：基于super_param处理后的数据进行分组汇总分析
#' # 1. 先通过super_param预处理数据（假设已完成）
#' # PARAMR <- super_param(data = raw_data, ...)
#'
#' # 2. 调用Tj_stats_Summary进行汇总与组间比较
#' Tj_stats_Summary(
#'   # 核心配置
#'   Stat = "Summary",          # 执行描述性统计与组间比较
#'   Object = "OA",         # 结果保存到变量"OA"中
#'
#'   # 数据配置：使用super_param处理后的结果
#'   data_param = list(
#'     super_param = PARAMR     # 传入预处理后的对象
#'   ),
#'
#'   # 变量配置：按"脓毒症"分组分析
#'   var_param = list(
#'     Group_by = "脓毒症"      # 以"脓毒症"为分组变量
#'     # 可选：exclude_var = c("冗余变量1", "冗余变量2")
#'   ),
#'
#'   # 模型参数：指定正态性假设与分析内容
#'   model_param = list(
#'     assume_normal_vars = c("diffday", "APACHEII"),  # 假设这两个变量符合正态分布
#'     perform_comparison = TRUE,     # 执行组间差异性检验
#'     perform_description = TRUE,    # 生成描述性统计
#'     perform_pairwise = TRUE        # 执行多组两两比较
#'   ),
#'
#'   # 输出参数：控制结果格式
#'   output_param = list(
#'     summary_display = TRUE,        # 显示汇总表格
#'     percent_type = "ROW",          # 百分比按行计算
#'     digits_percent = 4,            # 百分比保留4位小数
#'     digits_pvalue = 5              # P值保留5位小数
#'   )
#' )
#'
#' }
#'
#' @export
TjssnStat <- function (Stat = "Help", Object = "Param1", data_param = NULL,
                     var_param = NULL, model_param = NULL, output_param = NULL,
                     pack_param = NULL, logprint = T,plotprint=T, .call = NULL)
{
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("httr包安装...")
    install.packages("httr", dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  library(httr)
  if (is.null(.call)) {
    .call <- "RRR"
  }
  else {
    "RRR"
  }
  pack_infor <- list(.call = .call, Stat = Stat, Object = Object,
                     data_param = data_param, var_param = var_param, model_param = model_param,
                     output_param = output_param, pack_param = pack_param)
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file_path <- paste0(script_dir, "/send_docu1.rda")
  save(pack_infor, file = file_path, version = 2)
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file_path <- paste0(script_dir, "/send_docu1.rda")
  start_time <- Sys.time()
  analysis.response(file = file_path, obj = Object)
  end_time <- Sys.time()
  time_elapsed <- difftime(end_time, start_time, units = "secs")
  cat("\nTime:", time_elapsed, "sec\n")
  cat("\nSuccess\n")
  unlink(file_path)
  temp_file <- tempfile(fileext = ".rda")
  unlink(temp_file)
  if(isTRUE(logprint )){
    for (table_name in names(get(Object)[["result"]][["log.print"]][["summary"]][["result"]])) {
      cat(
        paste(get(Object)[["result"]][["log.print"]][["summary"]][["result"]][[table_name]], collapse = "\n"),  # 拼接当前表格的字符串向量
        "\n\n",
        sep = ""
      )
    }
  }
  if(isTRUE(plotprint )){
    for (table_name in names(get(Object)[["result"]][["visual"]][["ggplot"]])) {
      print(get(Object)[["result"]][["visual"]][["ggplot"]][[table_name]])
    }
  }
  return(invisible(NULL))
}
