.onAttach <- function(libname, pkgname) {
  # 定义依赖包
  required_packages <- c("dplyr", "officer", "flextable", "tidyverse", "httr", 
                         "patchwork", "ggplot2", "stringr", "readxl", "openxlsx")
  
  # 检查并安装缺失的包
  missing_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    message("检测到缺失的依赖包，正在自动安装：", paste(missing_pkgs, collapse = ", "))
    
    if (!"utils" %in% loadedNamespaces()) {
      requireNamespace("utils", quietly = TRUE)
    }
    utils::install.packages(
      missing_pkgs,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org/"
    )
  }
  
  # 修复：使用require确保返回逻辑值（TRUE/FALSE）
  loaded <- sapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(
      require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  })
  
  # 检查是否所有包都成功加载（确保loaded是逻辑向量）
  if (is.logical(loaded) && all(loaded)) {
    message("所有依赖包已成功加载")
  } else {
    failed <- names(loaded)[!loaded]
    warning("以下包加载失败：", paste(failed, collapse = ", "))
  }
  
  # 欢迎信息
  packageStartupMessage(
    "===== TJ packages loaded successfully =====\n",
    "📌 主要功能函数：\n",
    "  • super_param：数据变量综合处理与信息管理工具\n",
    "    → 查看使用方法：help(super_param)\n",
    "  • TJstats：统计分析核心功能（支持平台集成式分析，功能持续更新）\n",
    "    → 查看使用方法：help(TJstats)\n",
    "    → 查看可实现的模型分析：TJstats(Stat='help', Object='start.help')\n",
    "  • TJword：生成标准化Word统计报告（支持格式自定义）\n",
    "    → 查看使用方法：help(TJword)\n",
    "==========================================="
  )
}
