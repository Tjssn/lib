#' 生成Excel格式的变量处理配置模板
#'
#' 该函数用于自动生成标准化的Excel配置模板，包含变量信息表（var）和因子水平配置表（format），
#' 支持变量筛选、类型转换和命名规则设置。生成的模板可用于批量定义数据处理规则，
#' 并与`super_param`函数配合使用，实现标准化的数据预处理流程。
#'
#' @section 功能说明:
#' 函数主要完成以下工作：
#' 1. 根据筛选规则选择需要处理的变量
#' 2. 应用变量类型转换规则
#' 3. 按照指定命名规则处理变量名
#' 4. 自动识别定性/定量变量并分类
#' 5. 生成包含变量信息的"var"工作表和因子水平配置的"format"工作表
#' 6. 对Excel表格应用样式美化，提高可读性
#'
#' @param data 数据框(data.frame)，必填参数。用于生成配置模板的原始数据集。
#' @param output_file 字符型，可选参数。输出Excel文件的路径和名称，默认为"excel_super.xlsx"。
#'   支持相对路径和绝对路径，若目录不存在会自动创建。
#' @param factor_threshold 整数，可选参数。判断变量是否为因子型的阈值，默认值为10。
#'   当变量的唯一值数量小于等于该阈值时，自动识别为定性变量并在format表中生成水平配置。
#' @param keep_cols 字符向量，可选参数。指定需要保留的变量名称，格式为`c("变量1", "变量2")`。
#'   若设置此参数，仅保留指定变量并生成对应配置。
#' @param drop_cols 字符向量，可选参数。指定需要删除的变量名称，格式为`c("变量1", "变量2")`。
#'   若与`keep_cols`同时设置，`keep_cols`优先。
#' @param rename_safe 字符向量，可选参数。指定变量名的处理规则，可选值包括：
#'   "normal"：保留中文及合法字符（a-zA-Z0-9_），将非法符号转换为下划线；
#'   "original"：保留原始变量名，不做任何转换；
#'   "unicode"：将中文及特殊字符转换为Unicode编码（如"u4e2d"）。
#'   默认值为`c("normal", "original", "unicode")`，推荐使用"normal"兼顾可读性与兼容性。
#' @param retype.factor 字符向量，可选参数。指定需要转换为因子型的变量名向量。
#' @param retype.numeric 字符向量，可选参数。指定需要转换为数值型的变量名向量。
#' @param retype.character 字符向量，可选参数。指定需要转换为字符型的变量名向量。
#' @param retype.integer 字符向量，可选参数。指定需要转换为整数型的变量名向量。
#' @param retype.Date 字符向量，可选参数。指定需要转换为日期型的变量名向量。
#' @param retype.logical 字符向量，可选参数。指定需要转换为逻辑型的变量名向量。
#' @param retype.ordered 字符向量，可选参数。指定需要转换为有序因子的变量名向量。
#'
#' @details
#' 生成的Excel模板包含两个工作表：
#' - **var表**：包含变量基本信息，主要列说明：
#'   - ID_NUM：变量序号
#'   - dim：维度标识（默认为1）
#'   - cont：变量类型标识（1=定量变量，0=定性变量）
#'   ~  variable：***处理后的变量名（不允许修改！）*** ，与数据集中变量存在强关联，任何修改都会导致整个配置文件失效
#'   - label：变量标签（默认为变量名）
#'   - FMTNAME：因子水平配置关联标识（仅定性变量有值）
#'
#' - **format表**：包含因子水平配置，主要列说明：
#'   - FMTNAME：与var表关联的标识
#'   - type：水平值类型（N=数值型，C=字符型）
#'   - start：原始水平值
#'   - label：水平显示标签
#'   - Ref：是否为参照水平（T=是，<NA>=否）
#'
#' 表格样式按变量类型自动着色：
#' - 字符型：浅蓝色（#E3F2FD）
#' - 数值型：浅灰色（#D9D9D5）
#' - 因子型：浅橙色（#FFE8CC）
#' - 其他类型：极浅灰色（#F5F5F5）
#'
#' @return
#' 无形地返回一个列表，包含：
#' \code{var}：生成的变量信息表数据框
#' \code{format}：生成的因子水平配置表数据框
#' \code{data}：处理后的数据集（应用了筛选和类型转换）
#' 同时在指定路径生成Excel文件。
#'
#' @note
#' - 若指定路径已存在同名文件，函数会提示用户选择退出、生成新文件或覆盖现有文件
#' - 类型转换失败的变量会在控制台显示警告，但不会中断执行
#' - 生成的Excel模板可手动编辑后，通过`super_param`函数的`excel_super`参数导入，实现批量处理
#' - 依赖openxlsx包，若未安装会提示用户进行安装
#'
#' @examples
#' # 生成示例数据
#' set.seed(123)
#' df <- data.frame(
#'   ID = 1:100,
#'   年龄 = sample(18:70, 100, replace = TRUE),
#'   性别 = sample(c("男", "女"), 100, replace = TRUE),
#'   学历 = sample(c("高中", "本科", "硕士", "博士"), 100, replace = TRUE),
#'   收入 = sample(3000:20000, 100, replace = TRUE),
#'   入职日期 = as.Date("2020-01-01") + sample(0:1000, 100, replace = TRUE),
#'   在职状态 = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' )
#'
#' # 示例1：基础使用，生成默认模板
#' generate_excel_super(data = df)
#'
#' # 示例2：指定输出路径和因子阈值
#' generate_excel_super(
#'   data = df,
#'   output_file = "data_config/custom_template.xlsx",
#'   factor_threshold = 5  # 唯一值≤5的变量识别为定性变量
#' )
#'
#' # 示例3：筛选变量并指定类型转换
#' generate_excel_super(
#'   data = df,
#'   keep_cols = c("ID", "年龄", "性别", "学历"),  # 仅保留这些变量
#'   retype.factor = c("性别", "学历"),             # 转换为因子型
#'   retype.numeric = "年龄",                       # 确保为数值型
#'   rename_safe = "normal"                         # 使用常规命名规则
#' )
#'
#' @export
generate_excel_super <- function(data,
                                 output_file = "excel_super.xlsx",
                                 factor_threshold = 10,
                                 keep_cols = NULL,
                                 drop_cols = NULL,
                                 rename_safe = c("normal", "original", "unicode"),
                                 # retype.系列参数
                                 retype.factor = NULL,
                                 retype.numeric = NULL,
                                 retype.character = NULL,
                                 retype.integer = NULL,
                                 retype.Date = NULL,
                                 retype.logical = NULL,
                                 retype.ordered = NULL) {

  # 检查必要的包
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("需要安装openxlsx包，请运行：install.packages('openxlsx')")
  }

  # 匹配rename_safe参数
  rename_safe <- match.arg(rename_safe)

  # --------------------------
  # 1. 文件路径处理与安全检查
  # --------------------------
  file_dir <- dirname(output_file)
  file_name <- basename(output_file)
  file_ext <- tools::file_ext(file_name)
  file_prefix <- tools::file_path_sans_ext(file_name)

  if (!dir.exists(file_dir)) {
    dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
    message("已创建输出目录：", file_dir)
  }

  full_path <- file.path(file_dir, file_name)
  file_exists <- file.exists(full_path)

  if (file_exists) {
    cat("\n⚠️  检测到文件已存在：", full_path, "\n", sep = "")
    cat("请选择操作：\n")
    cat("1 - 退出执行（不生成文件）\n")
    cat("2 - 生成带时间尾缀的新文件（不覆盖原有文件）\n")
    cat("3 - 覆盖现有文件\n")

    while (TRUE) {
      user_choice <- readline(prompt = "请输入选项(1/2/3)：")
      if (user_choice %in% c("1", "2", "3")) break
      cat("输入无效，请重新输入1、2或3！\n")
    }

    if (user_choice == "1") {
      message("\n已退出执行，未生成文件")
      return(invisible(NULL))
    } else if (user_choice == "2") {
      time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
      new_file_name <- paste0(file_prefix, "_", time_suffix, ".", file_ext)
      full_path <- file.path(file_dir, new_file_name)
      message("\n将生成新文件：", full_path)
    } else {
      message("\n将覆盖现有文件：", full_path)
    }
  }

  # --------------------------
  # 2. 变量筛选
  # --------------------------
  vars <- names(data)

  if (!is.null(keep_cols)) {
    valid_keep <- intersect(keep_cols, vars)
    if (length(valid_keep) == 0) {
      warning("keep_cols中没有与数据匹配的变量，将使用所有变量")
      selected_vars <- vars
    } else {
      selected_vars <- valid_keep
    }
  } else if (!is.null(drop_cols)) {
    selected_vars <- setdiff(vars, drop_cols)
  } else {
    selected_vars <- vars
  }

  # 创建数据副本用于处理
  df <- data[, selected_vars, drop = FALSE]
  final_var_names <- names(df)

  # --------------------------
  # 处理类型转换 (修复前缀问题)
  # --------------------------
  # 创建空的数据框存储转换规则，避免unlist添加前缀
  conversion_rules <- data.frame(
    var = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # 添加各类型转换规则
  if (!is.null(retype.factor)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.factor, type = "factor", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.numeric)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.numeric, type = "numeric", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.character)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.character, type = "character", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.integer)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.integer, type = "integer", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.Date)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.Date, type = "Date", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.logical)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.logical, type = "logical", stringsAsFactors = FALSE))
  }
  if (!is.null(retype.ordered)) {
    conversion_rules <- rbind(conversion_rules,
                              data.frame(var = retype.ordered, type = "ordered", stringsAsFactors = FALSE))
  }

  if (nrow(conversion_rules) > 0) {
    message("\n🔄  开始执行变量类型转换...")
    success_conv <- character(0)

    # 检查有效的转换变量
    valid_retype_vars <- conversion_rules$var[conversion_rules$var %in% final_var_names]
    invalid_retype_vars <- conversion_rules$var[!conversion_rules$var %in% final_var_names]

    if (length(invalid_retype_vars) > 0) {
      message("ℹ️  跳过不存在的类型转换变量：", paste(invalid_retype_vars, collapse = ","))
    }

    # 安全执行函数
    safe_exec <- function(expr, msg) {
      tryCatch(
        expr,
        error = function(e) {
          warning(paste0("❌ [转换错误]", msg, "(错误：", e$message, ")"))
          return(NULL)
        }
      )
    }

    # 执行有效的类型转换
    for (i in seq_len(nrow(conversion_rules))) {
      var <- conversion_rules$var[i]
      type <- conversion_rules$type[i]

      # 只处理有效的变量
      if (!var %in% valid_retype_vars) next

      if (!type %in% c("numeric", "integer", "factor", "ordered", "Date", "character", "logical")) {
        warning(paste0("⚠️  不支持的类型'", type, "',变量'", var, "'转换失败"))
        next
      }

      converted <- safe_exec({
        switch(
          type,
          numeric = as.numeric(df[[var]]),
          integer = as.integer(df[[var]]),
          factor = as.factor(df[[var]]),
          ordered = as.ordered(df[[var]]),
          Date = as.Date(df[[var]]),
          character = as.character(df[[var]]),
          logical = as.logical(df[[var]])
        )
      }, paste("变量'", var, "'转换为'", type, "'失败"))

      if (!is.null(converted)) {
        df[[var]] <- converted
        success_conv <- c(success_conv, var)
        message(paste0("✅  变量'", var, "'成功转换为", type))
      }
    }
    message(paste0("📊  类型转换完成,成功转换", length(success_conv), "个变量"))
  }

  # --------------------------
  # 3. 变量名处理与类型识别
  # --------------------------
  process_var_names <- function(var_names, type, original_names) {
    switch(type,
           original = var_names,
           normal = {
             legal_chars <- "a-zA-Z0-9_\u4e00-\u9fa5"
             new_names <- gsub(paste0("[^", legal_chars, "]"), "_", var_names)
             gsub("_+", "_", new_names)
           },
           unicode = {
             sapply(var_names, function(char) {
               char_list <- strsplit(char, "")[[1]]
               processed <- sapply(char_list, function(c) {
                 if (grepl("[a-zA-Z0-9_]", c)) c
                 else paste0("u", as.hexmode(utf8ToInt(c)))
               })
               temp <- paste(processed, collapse = "")
               if (grepl("^[0-9]", temp)) temp <- paste0("v", temp)
               if (nchar(temp) == 0) temp <- paste0("var_", which(original_names == char))
               temp
             })
           }
    )
  }

  processed_vars <- process_var_names(selected_vars, rename_safe, names(data))
  names(processed_vars) <- selected_vars
  n_vars <- length(selected_vars)

  # 识别变量类型（用于底色区分）
  var_types <- sapply(selected_vars, function(var) {
    x <- df[[var]]  # 使用转换后的df
    if (is.factor(x)) "factor"
    else if (is.character(x)) "character"
    else if (is.numeric(x)) "numeric"
    else "other"
  })

  # --------------------------
  # 4. 构建var表格和format表格
  # --------------------------
  var_table <- data.frame(
    ID_NUM = 1:n_vars,
    dim = 1,
    cont = NA,
    variable = processed_vars,
    label = processed_vars,
    FMTNAME = NA,
    stringsAsFactors = FALSE
  )

  format_table <- data.frame(
    FMTNAME = character(),
    type = character(),
    start = character(),
    label = character(),
    Ref = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:n_vars) {
    orig_var_name <- selected_vars[i]
    var_name <- processed_vars[i]
    var_data <- df[[orig_var_name]]  # 使用转换后的df

    unique_vals <- unname(unique(var_data[!is.na(var_data)]))
    n_unique <- length(unique_vals)

    is_qualitative <- FALSE
    if (is.factor(var_data) || is.character(var_data) || n_unique <= factor_threshold) {
      is_qualitative <- TRUE
    }

    var_table$cont[i] <- ifelse(is_qualitative, 0, 1)

    if (is_qualitative) {
      var_table$FMTNAME[i] <- var_name

      type <- ifelse(all(grepl("^\\d+$", as.character(unique_vals))), "N", "C")

      levels_data <- data.frame(
        FMTNAME = var_name,
        type = type,
        start = as.character(unique_vals),
        label = as.character(unique_vals),
        Ref = ifelse(1:n_unique == 1, "T", ""),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      format_table <- rbind(format_table, levels_data)
    }
  }

  # --------------------------
  # 5. 定义样式
  # --------------------------
  # 表头样式：黑色背景、白色文字
  header_style <- openxlsx::createStyle(
    fgFill = "#000000",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#FFFFFF"
  )

  # 基础边框样式
  border_style <- openxlsx::createStyle(
    borderColour = "#4F81BD",
    border = "TopBottomLeftRight",
    halign = "center",
    valign = "center"
  )

  # 变量类型底色
  type_styles <- list(
    character = openxlsx::createStyle(fgFill = "#E3F2FD"),
    numeric = openxlsx::createStyle(fgFill = "#D9D9D5"),
    factor = openxlsx::createStyle(fgFill = "#FFE8CC"),
    other = openxlsx::createStyle(fgFill = "#F5F5F5")
  )

  # --------------------------
  # 6. 写入Excel并应用样式
  # --------------------------
  wb <- openxlsx::createWorkbook()

  # 处理var工作表
  openxlsx::addWorksheet(wb, "var")
  openxlsx::writeData(wb, "var", var_table, startRow = 1, startCol = 1)

  # 应用表头样式
  openxlsx::addStyle(wb, "var",
                     style = header_style,
                     rows = 1,
                     cols = 1:ncol(var_table),
                     gridExpand = TRUE)

  # 应用基础边框样式
  openxlsx::addStyle(wb, "var",
                     style = border_style,
                     rows = 2:(nrow(var_table) + 1),
                     cols = 1:ncol(var_table),
                     gridExpand = TRUE,
                     stack = TRUE)

  # 按变量类型应用底色
  if (any(var_types == "character")) {
    char_rows <- which(var_types == "character") + 1
    openxlsx::addStyle(wb, "var",
                       style = type_styles[["character"]],
                       rows = char_rows,
                       cols = 1:ncol(var_table),
                       gridExpand = TRUE,
                       stack = TRUE)
  }

  if (any(var_types == "numeric")) {
    num_rows <- which(var_types == "numeric") + 1
    openxlsx::addStyle(wb, "var",
                       style = type_styles[["numeric"]],
                       rows = num_rows,
                       cols = 1:ncol(var_table),
                       gridExpand = TRUE,
                       stack = TRUE)
  }

  if (any(var_types == "factor")) {
    factor_rows <- which(var_types == "factor") + 1
    openxlsx::addStyle(wb, "var",
                       style = type_styles[["factor"]],
                       rows = factor_rows,
                       cols = 1:ncol(var_table),
                       gridExpand = TRUE,
                       stack = TRUE)
  }

  # 调整列宽
  openxlsx::setColWidths(wb, "var", cols = 1:ncol(var_table),
                         widths = c(8, 6, 8, 15, 20, 15))

  # 处理format工作表
  openxlsx::addWorksheet(wb, "format")
  openxlsx::writeData(wb, "format", format_table, startRow = 1, startCol = 1)

  # 应用表头样式到format工作表
  openxlsx::addStyle(wb, "format", header_style, 1, 1:ncol(format_table), gridExpand = TRUE)
  if (nrow(format_table) > 0) {
    openxlsx::addStyle(wb, "format", border_style,
                       rows = 2:(nrow(format_table) + 1),
                       cols = 1:ncol(format_table),
                       gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::setColWidths(wb, "format", cols = 1:ncol(format_table),
                         widths = c(15, 8, 10, 20, 8))

  # 保存文件
  openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)

  # 输出信息
  message("\n✅  模板生成成功：", normalizePath(full_path))
  invisible(list(var = var_table, format = format_table, data = df))
}
