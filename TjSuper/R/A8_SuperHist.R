#' 批量绘制数值型变量直方图
#'
#' 该函数用于批量绘制数据集中所有数值型变量的直方图，并可选择添加密度曲线，
#' 结果按指定数量分面板展示并保存为PDF文件，方便快速了解多个数值变量的分布特征。
#'
#' @param data 数据框(data.frame)或tibble，必填参数。包含需要绘制直方图的数据集。
#' @param vars_per_panel 整数，可选参数，默认值为8。每个面板面板中显示的变量数量。
#' @param ncol 整数，可选参数，默认值为2。每个面板中的列数（即每行显示的直方图数量）。
#' @param binwidth 数值型，可选参数，默认值为NULL。直方图的组距，若为NULL则自动计算。
#' @param add_density 逻辑型，可选参数，默认值为TRUE。是否在直方图上叠加密度曲线。
#' @param output_file 字符型，可选参数，默认值为"histograms_check.pdf"。
#'   输出PDF文件的路径和文件名。
#'
#' @return 无返回值，函数会将绘制的直方图保存为指定的PDF文件。
#'
#' @details
#' 函数首先筛选数据集中的所有数值型变量，然后按`vars_per_panel`指定的数量分面板展示，
#' 每个面板内按`ncol`指定的列数排列直方图。对于存在缺失值的变量，会自动移除缺失值后绘制。
#' 若变量无有效数据（全部为缺失值），会显示"无有效数据"的提示文本。
#'
#' @examples
#' # 生成示例数据
#' set.seed(123)
#' df <- data.frame(
#'   age = c(25, 30, 35, 40, 45, NA, 50),
#'   height = rnorm(7, 170, 10),
#'   weight = rnorm(7, 65, 5),
#'   score = c(80, 85, NA, 90, 95, 75, 88),
#'   income = rlnorm(7, 10, 0.5)
#' )
#'
#' # 绘制直方图并保存为默认文件
#' plot_variable_histograms(data = df)
#'
#' # 自定义参数：每个面板显示4个变量，3列布局，不添加密度曲线
#' plot_variable_histograms(
#'   data = df,
#'   vars_per_panel = 4,
#'   ncol = 3,
#'   add_density = FALSE,
#'   output_file = "custom_histograms.pdf"
#' )
#'
#' @export
SuperHist <- function(data,
                                     vars_per_panel = 8,
                                     ncol = 2,
                                     binwidth = NULL,
                                     add_density = TRUE,
                                     output_file = "histograms_check.pdf") {

  # 筛选数值型变量
  numeric_vars <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_vars, drop = FALSE]
  var_names <- names(data_numeric)

  if (length(var_names) == 0) {
    message("数据集中没有数值型变量可绘制直方图")
    return(invisible(NULL))
  }

  # 计算总面板数
  total_vars <- length(var_names)
  total_panels <- ceiling(total_vars / vars_per_panel)

  # 创建PDF文件（使用默认字体）
  pdf(output_file,
      width = 10,
      height = 8,
      family = "GB1")

  # 循环每个面板
  for (panel in 1:total_panels) {
    start_idx <- (panel - 1) * vars_per_panel + 1
    end_idx <- min(panel * vars_per_panel, total_vars)
    current_vars <- var_names[start_idx:end_idx]
    current_n <- length(current_vars)
    current_nrow <- ceiling(current_n / ncol)

    # 设置面板布局（指定字体）
    par(mfrow = c(current_nrow, ncol),
        mar = c(4, 4, 2, 1),
        mgp = c(2, 0.8, 0))

    # 绘制直方图
    for (var in current_vars) {
      x <- na.omit(data_numeric[[var]])

      if (length(x) == 0) {
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = var)
        text(0, 0, "无有效数据", col = "red")
        next
      }

      hist(x,
           main = var,
           xlab = var,
           col = "lightblue",
           border = "white",
           binwidth = binwidth,
           freq = TRUE)

      if (add_density) {
        lines(density(x), col = "red", lwd = 2)
      }
    }
  }

  dev.off()
  message(paste("直方图已保存至:", output_file))
}
