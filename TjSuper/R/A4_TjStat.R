#' TjssnStat
#'
#' 主分析函数，支持多种统计分析功能，通过参数配置实现数据处理、模型构建和结果输出，
#' 支持直接赋值获取分析结果。
#'
#' @param Stat 字符型，可选参数，默认值为 `"help"`。指定要执行的统计分析类型，
#'   如描述性统计、回归分析等。
#' @param data_param 列表，可选参数。数据相关参数配置。
#' @param var_param 列表，可选参数。变量相关参数配置。
#' @param model_param 列表，可选参数。模型相关参数配置。
#' @param output_param 列表，可选参数。输出相关参数配置。
#' @param pack_param 列表，可选参数。打包传输相关参数配置。
#' @param logprint 逻辑型，可选参数，默认值为 `TRUE`。是否打印分析过程日志信息。
#'   如果返回对象中存在 `result$log.print$Quick.Check.Text`，则自动使用 `cat()`
#'   打印该文本。
#' @param plotprint 逻辑型，可选参数，默认值为 `TRUE`。是否自动打印生成的图形结果。
#' @param .call 字符型，可选参数。内部调用标识，用于指定分析引擎调用方式。
#'
#' @return 返回分析结果列表。若 `logprint = TRUE` 且返回对象中存在
#'   `result$log.print$Quick.Check.Text`，则会在控制台打印 Quick.Check 日志。
#'
#' @export
TjssnStat <- function(
    Stat = "help",
    data_param = NULL,
    var_param = NULL,
    model_param = NULL,
    output_param = NULL,
    pack_param = NULL,
    logprint = TRUE,
    plotprint = TRUE,
    .call = NULL
) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Please install it first.", call. = FALSE)
  }

  if (is.null(.call)) {
    .call <- "RRR"
  }

  pack_infor <- list(
    Stat = Stat,
    data_param = data_param,
    var_param = var_param,
    model_param = model_param,
    output_param = output_param,
    pack_param = pack_param,
    .call = .call
  )

  script_dir <- getwd()

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(
      rstudioapi::getSourceEditorContext(),
      error = function(e) NULL
    )

    if (!is.null(ctx) && !is.null(ctx$path) && nzchar(ctx$path)) {
      script_dir <- dirname(ctx$path)
    }
  }

  file_path <- file.path(script_dir, "send_docu1.rda")
  save(pack_infor, file = file_path, version = 2)

  start_time <- Sys.time()

  result <- tryCatch({
    TJanalysis.response(file = file_path)
  }, error = function(e) {
    message("TjssnStat analysis failed: ", conditionMessage(e))
    NULL
  })

  end_time <- Sys.time()
  time_elapsed <- difftime(end_time, start_time, units = "secs")

  if (file.exists(file_path)) {
    unlink(file_path)
  }

  if (isTRUE(logprint) &&
      !is.null(result) &&
      !is.null(result$result) &&
      !is.null(result$result$log.print) &&
      !is.null(result$result$log.print$Quick.Check.Text)) {

    quick_check_text <- result$result$log.print$Quick.Check.Text

    if (length(quick_check_text) > 0 &&
        any(!is.na(quick_check_text)) &&
        any(nzchar(trimws(as.character(quick_check_text))))) {

      cat(paste(as.character(quick_check_text), collapse = "\n"))
      cat("\n")
    }
  }

  if (isTRUE(plotprint) &&
      !is.null(result) &&
      !is.null(result$result) &&
      !is.null(result$result$visual) &&
      !is.null(result$result$visual$ggplot)) {

    plot_list <- result$result$visual$ggplot

    if (length(plot_list) > 0) {
      for (plot_name in names(plot_list)) {
        print(plot_list[[plot_name]])
      }
    }
  }

  return(result)
}
