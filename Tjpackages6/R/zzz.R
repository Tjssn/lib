# 当用户加载包时触发（library(你的包名)时执行）
.onAttach <- function(libname, pkgname) {
  # 定义你的包依赖的必要包
  required_packages <- c("dplyr", "officer", "flextable", "tidyverse", "httr", 
                         "patchwork", "ggplot2", "stringr", "readxl", "openxlsx")
  
  # 检查并安装缺失的包
  missing_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    # 提示用户需要安装缺失的包
    message("检测到缺失的依赖包，正在自动安装：", paste(missing_pkgs, collapse = ", "))
    
    # 安装缺失的包（使用CRAN镜像）
    if (!"utils" %in% loadedNamespaces()) {
      requireNamespace("utils", quietly = TRUE)
    }
    utils::install.packages(
      missing_pkgs,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org/"  # 指定CRAN镜像，避免用户未设置
    )
  }
  
  # 加载所有依赖包
  loaded <- sapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  })
  
  # 检查是否所有包都成功加载
  if (all(loaded)) {
    message("所有依赖包已成功加载")
  } else {
    failed <- names(loaded)[!loaded]
    warning("以下包加载失败：", paste(failed, collapse = ", "))
  }
  
  # 可选：欢迎用户的欢迎信息
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
 .onAttach()
