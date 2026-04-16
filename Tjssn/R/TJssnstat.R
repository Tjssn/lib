#' TjssnStat
#'
#' 主分析函数，支持多种统计分析功能，通过参数配置实现数据处理、模型构建和结果输出，
#' 支持直接赋值获取分析结果。
#'
#' @param Stat 字符型，可选参数，默认值为"help"。指定要执行的统计分析类型，如描述性统计、回归分析等，
#'   具体支持的分析类型可参考帮助文档。
#' @param Data_param 列表，可选参数。数据相关参数配置，包含：
#'   - 原始数据集或数据路径
#'   - 数据筛选条件（如行筛选逻辑）
#'   - 缺失值处理方式
#'   - 数据转换规则（如标准化、中心化）等
#' @param Var_param 列表，可选参数。变量相关参数配置，包含：
#'   - 变量角色指定（自变量、因变量、调节变量等）
#'   - 变量类型定义（连续型、分类型等）
#'   - 变量标签与领域分类信息
#'   - 因子变量的水平设置等
#' @param Model_param 列表，可选参数。模型相关参数配置，包含：
#'   - 模型类型（如线性回归、logistic回归等）
#'   - 算法参数（如惩罚项系数、迭代次数等）
#'   - 显著性水平阈值
#'   - 置信区间设置等
#' @param Output_param 列表，可选参数。输出相关参数配置，包含：
#'   - 结果保存路径与文件名
#'   - 输出格式（表格为CSV/Excel，图形为PNG/PDF等）
#'   - 表格样式设置（如是否显示行名、小数位数）
#'   - 图形参数（尺寸、分辨率、主题等）
#' @param Pack_param 列表，可选参数。打包传输相关参数配置，包含：
#'   - 数据压缩开关
#'   - 传输加密设置
#'   - 超时时间配置
#'   - 中间文件保存选项等
#' @param .call 字符型，可选参数，默认值为"RRR"。内部调用标识，用于指定分析引擎调用方式。
#'
#' @return 分析结果列表，包含统计结果、日志信息、图形对象等，具体结构因分析类型而异。
#'
#' @export

TjssnStat <- function(Stat = "help", Data_param = NULL,
                      Var_param = NULL, Model_param = NULL,
                      Output_param = NULL, Pack_param = NULL,
                      .call = NULL) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    message("httr...")
    install.packages("httr", dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  library(httr)

  if (is.null(.call)) {
    .call <- "RRR"
  }

  pack_infor <- list(
    Stat = Stat,
    data_param = Data_param,
    var_param = Var_param,
    model_param = Model_param,
    output_param = Output_param,
    pack_param = Pack_param,
    .call = .call
  )

  script_dir <- if (!is.null(rstudioapi::getSourceEditorContext())) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
  file_path <- file.path(script_dir, "send_docu1.rda")
  save(pack_infor, file = file_path, version = 2)
  start_time <- Sys.time()
  result <- tryCatch({
    TJanalysis.response(file = file_path)
  }, error = function(e) {
    NULL
  })
  end_time <- Sys.time()
  time_elapsed <- difftime(end_time, start_time, units = "secs")
  if (file.exists(file_path)) {
    unlink(file_path)
  }
  cat("Success!")
  return(result)
}
