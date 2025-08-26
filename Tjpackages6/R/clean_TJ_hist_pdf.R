
plot_variable_histograms <- function(data,
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
