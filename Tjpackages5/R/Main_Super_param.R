#' super_param
#'
#' 对输入数据框进行变量处理与管理，支持变量重命名、类型转换、因子水平设置、变量筛选（保留/删除）、
#' 标签管理等功能，并可通过Excel配置表实现批量处理。生成详细的变量信息汇总表和清洗日志，
#' 支持表格着色与动态排序，为后续数据分析提供标准化数据集，同时通过Viewer实时可视化处理结果，
#' 方便数据质量评估与预处理流程追溯。
#'
#' @section 重要提示:
#' \strong{⚠️} 函数执行过程会生成详细的清洗日志，记录变量处理的每一步操作、转换结果及潜在问题，
#' 建议仔细查看日志信息以确保数据处理符合预期。参数间存在优先级关系（如`keep_cols`优先于`drop_cols`），
#' 请留意参数间的相互影响。
#'
#' @param data 数据框(data.frame)，必填参数。待处理的原始数据集，包含需要进行清洗和转换的所有变量。
#' @param rename_theme 字符向量，可选参数。指定最终变量名的初始格式，决定变量名的基础样式，可选值包括：
#'   "unicode"：将中文及所有非法字符转换为Unicode编码（格式为"u+十六进制值"），仅保留a-zA-Z0-9_等合法字符；
#'   "normal"：保留中文及合法字符（a-zA-Z0-9_），将所有非法符号转换为下划线；
#'   "original"：完全保留原始变量名，不做任何转换。
#'   默认值为`c("unicode", "normal", "original")`。
#' @param rename 命名向量，可选参数。用于自定义变量重命名规则，格式为`c("旧最终变量名"="新最终变量名")`。
#'   该操作将覆盖由`rename_theme`参数设置的初始最终变量名，重命名失败的变量会在日志中提示。
#' @param relabel 命名向量，可选参数。用于修改变量的标签信息，格式为`c("最终变量名"="新label")`。
#'   标签主要用于变量信息表中的展示，不影响变量实际名称和数据处理。
#' @param retype.factor 字符向量，可选参数。指定需要转换为因子型的变量名向量。
#' @param retype.numeric 字符向量，可选参数。指定需要转换为数值型的变量名向量。
#' @param retype.character 字符向量，可选参数。指定需要转换为字符型的变量名向量。
#' @param retype.integer 字符向量，可选参数。指定需要转换为整数型的变量名向量。
#' @param retype.Date 字符向量，可选参数。指定需要转换为日期型的变量名向量。
#' @param retype.logical 字符向量，可选参数。指定需要转换为逻辑型的变量名向量。
#' @param factor_levels 列表，可选参数。用于自定义因子变量的水平设置，格式为`list("最终变量名"=list(levels=, labels=, ordered=, ref_levels=))`。
#'   其中：
#'   - levels：必须项，指定水平顺序；
#'   - labels：可选项，水平标签（默认与levels一致）；
#'   - ordered：可选项，是否为有序因子（默认FALSE）；
#'   - ref_levels：可选项，指定参照水平（支持通过水平值或标签设置）。
#' @param other_cols 字符向量，可选参数。标记为"其他"类别的变量名称（基于`最终变量名`），在动态排序中
#'   这些变量将展示在特定区域，可用于区分特殊类别的变量。
#' @param add_mark 列表，可选参数。用于在变量信息汇总表中添加自定义标记列，
#'   格式为`list(<标记列名1>=<标记规则1>, <标记列名2>=<标记规则2>)`，其中标记规则支持tidy的mutate语法。
#'   若标记列包含"dim"，未匹配的变量将使用`default_mark`填充。
#' @param domain_mark 列表，可选参数。领域分类映射表，格式为`list(领域名称=变量向量)`，用于将变量归类到不同领域。
#' @param default_mark 可选参数。当`add_mark`中包含"dim"列且部分变量未匹配到标记规则时，
#'   使用此值作为默认填充，默认值为NA。
#' @param dynamic_view_mark 逻辑型，可选参数。是否启用变量动态排序功能。若为TRUE（默认），
#'   变量顺序为"其他类别变量(other_cols) > 经过修改的变量 > 其他变量"；若为FALSE，保持原始变量顺序。
#'   若`sort_mark`已设置，则`dynamic_view_mark`自动失效。
#' @param sort_mark 字符向量，可选参数。指定用于排序的变量名（基于变量信息汇总表的列名），
#'   变量信息表将按指定列的取值排序，优先级高于`dynamic_view_mark`。若指定列不存在，将在日志中警告并使用默认排序。
#' @param max_level 整数，可选参数。控制变量值展示的最大水平数量，超过该数量时仅显示前N个水平
#'   并提示总数，默认值为5。
#' @param max_level_pos 整数，可选参数。控制变量值展示的最大字符长度，超过该长度将截断并添加"..."，
#'   默认值为30。
#' @param color_col_mark 字符向量，可选参数。指定需要进行颜色标记的列名（基于变量信息汇总表），
#'   函数会为该列的不同水平自动分配区分度良好的颜色，增强表格可读性。
#' @param keep_cols 字符向量，可选参数。指定需要从数据中保留的变量名称（基于最终变量名），
#'   格式为`c("最终变量名1", "最终变量名2")`。保留操作在类型转换和因子设置之后执行，
#'   优先级高于`drop_cols`。
#' @param drop_cols 字符向量，可选参数。指定需要从数据中删除的变量名称（基于最终变量名），
#'   格式为`c("最终变量名1", "最终变量名2")`。删除操作在类型转换和因子设置之后执行。
#'   若与`keep_cols`同时设置，`keep_cols`优先。
#' @param excel_super 列表，可选参数。用于导入Excel格式的批量处理配置表，格式为`list(var=..., format=...)`，其中：
#'   - var：包含"ID_NUM"、"dim"、"label"、"cont"、"variable"等列的数据框；
#'   - format：包含因子水平配置的辅助数据框，需包含"FMTNAME"、"start"、"label"、"Ref"等列。
#'   若设置此参数，将解析配置表并补充`rename`、`relabel`、`retype`、`factor_levels`等参数的设置，
#'   手动设置的参数优先级高于Excel配置。
#' @param excel_inherit_types 逻辑型，可选参数。是否从Excel配置表继承类型规则，默认为FALSE。
#'   若为TRUE，将补充Excel中定义的数值型和因子型变量（不覆盖手动设置的类型转换规则）。
#'
#' @details
#' 变量处理优先级：\cr
#' 1. 批量配置解析（`excel_super`）→ 2. 初始变量名生成（`rename_theme`）→ 3. 自定义重命名（`rename`）→ \cr
#' 4. 标签修改（`relabel`）→ 5. 类型转换（`retype.*`系列参数）→ 6. 因子水平设置（`factor_levels`）→ \cr
#' 7. 变量筛选（`keep_cols`优先于`drop_cols`）→ 8. 排序（`sort_mark`优先于`dynamic_view_mark`）。\cr
#'
#' 表格着色逻辑：通过`color_col_mark`指定列后，函数会为该列的每个唯一水平分配不同的色调，
#' 增强不同类别间的视觉区分度，未指定时默认按变量类型（字符型/数值型/因子型）着色。
#'
#' 日志信息包含：各步骤操作结果、警告信息、错误原因等，便于追踪处理过程和排查问题。
#'
#' @return
#' 列表，包含以下核心组件：
#' \code{data}: 处理后的标准化数据集，变量按指定顺序排列；
#' \code{label_mapping}: 变量名映射表，包含original、normal、unicode、label等信息；
#' \code{var_info}: 变量信息列表，包括统计信息（总观测数、缺失值数量等）和类型信息（基础类型、继承类型等）；
#' \code{Viewer}: 包含格式化的变量信息汇总表，展示变量的各类属性和统计特征，支持颜色标记；
#' \code{cleaning_log}: 清洗过程的详细日志，记录每一步操作的结果和警告信息；
#' \code{stats}: 数据处理前后的统计对比，包括样本量、变量数、删除变量数等。
#'
#' @note
#' 建议在处理大型数据集前先进行测试，以评估函数性能；\cr
#' 变量名若包含特殊字符，会被自动转换为安全格式，转换规则可通过`rename_theme`参数控制；\cr
#' 因子水平设置时，请确保`levels`参数包含所有可能的值，否则会产生NA值；\cr
#' 类型转换可能导致数据丢失（如字符型转数值型时的非数字值），请在日志中确认转换成功率。
#'
#' @author
#' 开发团队:TjssnStat团队;\cr
#' 联系方式:VX:Tongjissn;\cr
#' 官方网址:\url{https://study.tjbbb.com};\cr
#' 微信:Tongjissn;\cr
#' 官方平台-公众号:统计碎碎念
#'
#' @examples
#' # 准备工作：加载数据和包
#' rm(list = ls()); gc()
#' # library(Tjssn)
#' # 模拟数据
#' df <- data.frame(
#'   ID = 1:100,
#'   年龄 = c(25, 30, 35, NA, 40),
#'   性别 = factor(c(1, 2, 1, 2, 1), levels = 1:2),
#'   血压 = c("120/80", "130/85", "140/90", NA, "110/70"),
#'   确诊日期 = as.Date(c("2023-01-01", "2023-02-15", NA, "2023-03-10", "2023-04-05"))
#' )
#'
#' # 示例1：基础使用，查看数据处理结果
#' dat.param1 <- super_param(data = df)
#' # 查看处理后的数据和变量信息表
#' head(dat.param1$data)
#' dat.param1$Viewer$table
#'
#' # 示例2：指定变量名规则并标记其他类别变量
#' dat.param2 <- super_param(
#'   data = df,
#'   rename_theme = "normal",  # 生成保留中文和合法字符的变量名
#'   other_cols = c("ID", "性别")  # 存储唯一标识符，日期，面板数据时间指示变量等其他变量。
#' )
#'
#' # 示例3：自定义重命名、标签与类型转换
#' dat.param3 <- super_param(
#'   data = df,
#'   rename_theme = "normal",
#'   other_cols = "ID",
#'   rename = c(血压 = "blood_pressure", 确诊日期 = "diagnosis_date"),  # 自定义重命名
#'   relabel = c(blood_pressure = "血压(mmHg)", 年龄 = "年龄(岁)"),  # 修改变量标签
#'   retype.numeric = "年龄",  # 转换为数值型
#'   retype.factor = "性别"    # 转换为因子型
#' )
#'
#' # 示例4：设置因子水平与参照组
#' dat.param4 <- super_param(
#'   data = df,
#'   rename_theme = "normal",
#'   other_cols = "ID",
#'   rename = c(性别 = "gender", 年龄 = "age"),
#'   relabel = c(gender = "性别", age = "年龄(岁)"),
#'   retype.numeric = "age",
#'   retype.factor = "gender",
#'   # 自定义因子水平与参照组
#'   factor_levels = list(
#'     gender = list(levels = c(1, 2), labels = c("男", "女"), ref_levels = "男")
#'   )
#' )
#'
#' # 示例5：变量筛选与排序
#' dat.param5 <- super_param(
#'   data = df,
#'   rename_theme = "normal",
#'   keep_cols = c("ID", "年龄", "性别"),  # 仅保留指定变量
#'   sort_mark = "年龄"  # 按年龄排序
#' )
#'
#' @export
super_param <- function(
    data,                          # 输入数据框（必选）
    # 变量筛选
    keep_cols = NULL,              # 保留指定变量（优先级高于drop_cols）
    drop_cols = NULL,              # 删除指定变量
    # 命名与标签
    rename_theme = c("unicode", "normal", "original"),  # 变量命名规则
    rename = NULL,                 # 自定义变量重命名（命名向量：原名=新名）
    relabel = NULL,                # 自定义变量标签（命名向量：变量名=标签）
    # 类型转换（R用户手动设置，优先级最高）
    retype.factor = NULL,          # 转换为因子型的变量名向量
    retype.numeric = NULL,         # 转换为数值型的变量名向量
    retype.character = NULL,       # 转换为字符型的变量名向量
    retype.integer = NULL,         # 转换为整数型的变量名向量
    retype.Date = NULL,            # 转换为日期型的变量名向量
    retype.logical = NULL,         # 转换为逻辑型的变量名向量
    # 因子水平设置
    factor_levels = NULL,          # 因子水平配置（命名列表：变量名=list(levels=, labels=, ref_levels=)）
    # 变量分类与标记
    other_cols = NULL,             # 标记为"其他"类别的变量
    add_mark = NULL,               # 附加标记列配置
    domain_mark = NULL,            # 领域分类映射表（列表：领域=变量向量）
    default_mark = NA,             # 未匹配标记时的默认值
    # 展示设置
    dynamic_view_mark = TRUE,      # 是否启用动态视图排序
    sort_mark = NULL,              # 表格排序依据列
    max_level = 5,                 # 变量水平最大展示数量
    max_level_pos = 30,            # 变量水平展示的最大字符长度
    color_col_mark = NULL,         # 需要着色的表格列
    # Excel配置（与R手动设置分离）
    excel_super = NULL,            # Excel配置列表（含var和format表格）
    excel_inherit_types = FALSE    # 是否从Excel继承类型规则（默认不继承）
){
  # 定义状态图标
  icon_info <- "\U27A1\UFE0F"    # 信息箭头
  icon_success <- "\u2705"       # 成功对勾
  icon_warning <- "\U26A0\UFE0F" # 警告三角
  icon_error <- "\U274C"         # 错误叉号

  # 生成区分度良好的颜色函数
  get_distinct_colors <- function(n) {
    if (n <= 1) return("#E0E0E0")
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  # 处理变量名冲突的辅助函数
  resolve_name_conflict <- function(new_name, existing_names) {
    if (!(new_name %in% existing_names)) {
      return(new_name)
    }
    pattern <- paste0("^", new_name, "_(\\d+)$")
    matches <- grep(pattern, existing_names, value = TRUE)
    numbers <- as.integer(sub(pattern, "\\1", matches))
    if (length(numbers) == 0) {
      return(paste0(new_name, "_2"))
    } else {
      return(paste0(new_name, "_", max(numbers) + 1))
    }
  }

  # 核心表格列定义
  core_cols <- c("最终变量名", "变量名标签", "基础类型", "继承类型",
                 "有效/缺失", "变量值", "变量值域", "domain", "domain_rank")

  # 检查必要的包
  # 检查并加载flextable包
  if (!requireNamespace("flextable", quietly = TRUE)) {
    warning("flextable包未安装，正在从CRAN下载安装...")
    install.packages("flextable", repos = "https://cran.r-project.org")
  }
  library(flextable)

  # 检查输入数据有效性
  if (!inherits(data, "data.frame")) {
    warning(paste0(icon_error, " 输入数据必须为数据框,返回NULL"))
    return(NULL)
  }
  df <- data  # 原始数据副本
  init_raw_vars <- names(df)
  final_var_names <- init_raw_vars
  init_nrow <- nrow(df)
  init_ncol <- ncol(df)

  # 初始化清洗日志
  cleaning_log <- character(0)
  cleaning_log <- c(cleaning_log, paste0(icon_info, " 原始变量名：", paste(init_raw_vars, collapse = ",")))

  # 安全执行函数（捕获错误）
  safe_exec <- function(expr, msg) {
    tryCatch(
      expr,
      error = function(e) {
        warning(paste0(icon_error, " [执行错误] ", msg, "（原因：", e$message, "）"))
        cleaning_log <<- c(cleaning_log, paste0(icon_error, " [执行错误] ", msg, "（原因：", e$message, "）"))
        return(NULL)
      }
    )
  }

  # 变量名转换辅助函数（处理特殊字符）- 仅保留这一个正确定义
  get_universal_name <- function(char, old_names) {
    # 处理空字符或NULL情况
    if (is.null(char) || nchar(char) == 0) {
      return(paste0("var_", which(old_names == char)))
    }

    char_list <- strsplit(char, "")[[1]]
    # 确保processed变量初始化
    processed <- character(0)
    processed <- sapply(char_list, function(c) {
      if (grepl("[a-zA-Z0-9_]", c)) {
        c  # 保留合法字符
      } else {
        # 转换特殊字符为Unicode编码
        int_val <- utf8ToInt(c)
        if (is.na(int_val)) "uFFFD" else paste0("u", as.hexmode(int_val))
      }
    })

    # 处理可能的空结果
    if (length(processed) == 0) {
      return(paste0("var_", which(old_names == char)))
    }

    temp <- paste(processed, collapse = "")
    # 确保变量名不以数字开头
    if (grepl("^[0-9]", temp)) {
      temp <- paste0("v", temp)
    }
    # 处理空结果
    if (nchar(temp) == 0) {
      temp <- paste0("var_", which(old_names == char))
    }
    temp
  }

  # ==============================================
  # 1. 处理Excel配置（与R手动设置完全分离）
  # ==============================================
  excel_types <- list(factor = NULL, numeric = NULL)  # 存储Excel中的类型规则
  # 新增：存储Excel中的dim和ID_NUM映射
  excel_domain_mapping <- NULL
  # 新增：存储Excel中的其他列
  excel_additional_cols <- NULL

  if (!is.null(excel_super)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 开始处理excel_super配置"))

    if (!is.list(excel_super) || !all(c("var", "format") %in% names(excel_super))) {
      warning(paste0(icon_error, " excel_super必须是包含'var'和'format'的列表"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " excel_super格式错误，跳过处理"))
      excel_super <- NULL
    } else {
      var_sheet <- excel_super$var
      format_sheet <- excel_super$format

      # 处理var表格（变量信息）
      if (inherits(var_sheet, "data.frame")) {
        required_var_cols <- c("ID_NUM", "dim", "label", "cont", "variable")
        missing_cols <- setdiff(required_var_cols, colnames(var_sheet))
        if (length(missing_cols) > 0) {
          warning(paste0(icon_error, " var表格缺少必要列：", paste(missing_cols, collapse = ",")))
          cleaning_log <- c(cleaning_log, paste0(icon_error, " var表格列不完整，跳过处理"))
          var_sheet <- NULL
        } else {
          # 新增：保存dim和ID_NUM的映射关系
          excel_domain_mapping <- data.frame(
            variable = var_sheet$variable,
            dim = var_sheet$dim,
            ID_NUM = var_sheet$ID_NUM,
            stringsAsFactors = FALSE
          )

          # 新增：提取并保存其他列
          additional_col_names <- setdiff(colnames(var_sheet), required_var_cols)
          if (length(additional_col_names) > 0) {
            excel_additional_cols <- var_sheet[, c("variable", additional_col_names), drop = FALSE]
            cleaning_log <- c(cleaning_log, paste0(icon_success, " 从Excel提取额外列：",
                                                   paste(additional_col_names, collapse = ",")))
          }

          # 加载变量标签（不覆盖手动设置）
          excel_relabel <- setNames(var_sheet$label, var_sheet$variable)
          relabel <- if (is.null(relabel)) {
            excel_relabel
          } else {
            c(excel_relabel[!names(excel_relabel) %in% names(relabel)], relabel)
          }
          cleaning_log <- c(cleaning_log, paste0(icon_success, " 从Excel加载变量标签：", length(excel_relabel), "个"))

          # 提取Excel类型规则（仅存储，不自动应用）
          excel_vars <- var_sheet$variable
          excel_types$numeric <- excel_vars[var_sheet$cont == 1]  # cont=1视为数值
          excel_types$factor <- excel_vars[var_sheet$cont != 1]   # cont≠1视为因子
          cleaning_log <- c(cleaning_log, paste0(icon_info, " Excel类型规则：",
                                                 length(excel_types$numeric), "个numeric，",
                                                 length(excel_types$factor), "个factor"))
        }
      }

      # 处理format表格（因子水平，不影响类型转换）
      if (inherits(format_sheet, "data.frame") && inherits(var_sheet, "data.frame")) {
        required_format_cols <- c("FMTNAME", "start", "label", "Ref")
        missing_cols <- setdiff(required_format_cols, colnames(format_sheet))
        if (length(missing_cols) > 0) {
          warning(paste0(icon_error, " format表格缺少必要列：", paste(missing_cols, collapse = ",")))
          cleaning_log <- c(cleaning_log, paste0(icon_error, " format表格列不完整，跳过处理"))
        } else {
          colnames(format_sheet)[colnames(format_sheet) == "label"] <- "format_label"
          format_vars <- unique(format_sheet$FMTNAME)
          valid_vars <- intersect(format_vars, var_sheet$variable)

          excel_factor_levels <- list()
          for (var in valid_vars) {
            format_sub <- subset(format_sheet, FMTNAME == var)
            excel_factor_levels[[var]] <- list(
              levels = format_sub$start,
              labels = format_sub$format_label,
              ref_levels = ifelse(any(format_sub$Ref == "T"),
                                  format_sub$format_label[format_sub$Ref == "T"][1],
                                  NULL)
            )
          }
          # 合并因子水平（手动设置优先）
          factor_levels <- if (is.null(factor_levels)) {
            excel_factor_levels
          } else {
            c(excel_factor_levels[!names(excel_factor_levels) %in% names(factor_levels)], factor_levels)
          }
          cleaning_log <- c(cleaning_log, paste0(icon_success, " 从Excel加载因子水平：", length(valid_vars), "个变量"))
        }
      }
    }
  }

  # ==============================================
  # 2. 变量命名规则处理
  # ==============================================
  rename_theme <- match.arg(rename_theme)
  # 生成不同格式的变量名
  legal_chars <- "a-zA-Z0-9_\u4e00-\u9fa5"
  normal_names <- sapply(init_raw_vars, function(name) {
    gsub("_+", "_", gsub(paste0("[^", legal_chars, "]"), "_", name))
  })
  normal_names <- make.unique(normal_names, sep = "_")

  unicode_names <- sapply(init_raw_vars, function(x) get_universal_name(x, init_raw_vars))
  unicode_names <- make.unique(unicode_names, sep = "_")

  # 创建标签映射表
  label_mapping <- data.frame(
    original = init_raw_vars,
    normal = normal_names,
    unicode = unicode_names,
    label = init_raw_vars,  # 初始标签为变量名
    stringsAsFactors = FALSE
  )

  # 应用初始命名规则
  initial_final_names <- switch(
    rename_theme,
    "original" = label_mapping$original,
    "normal" = label_mapping$normal,
    "unicode" = label_mapping$unicode
  )
  names(df) <- initial_final_names
  final_var_names <- names(df)
  cleaning_log <- c(cleaning_log, paste0(icon_success, " 应用命名规则（", rename_theme, "）：",
                                         paste(initial_final_names, collapse = ",")))

  # 处理自定义重命名（手动重命名优先）
  if (!is.null(rename)) {
    if (is.null(names(rename)) || any(names(rename) == "")) {
      warning(paste0(icon_error, " rename必须是命名向量（名称为原始变量名，值为新名）"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " rename格式错误，跳过"))
    } else {
      exist_vars <- names(rename) %in% final_var_names
      if (any(!exist_vars)) {
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的重命名变量：",
                                               paste(names(rename)[!exist_vars], collapse = ",")))
        rename <- rename[exist_vars]
      }
      if (length(rename) > 0) {
        names(df)[match(names(rename), final_var_names)] <- rename
        final_var_names <- names(df)
        label_mapping$label[match(names(rename), label_mapping$original)] <- unname(rename)
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 完成自定义重命名：",
                                               paste(paste(names(rename), rename, sep="→"), collapse = ",")))
      }
    }
  }
  original_order <- final_var_names  # 记录原始顺序

  # ==============================================
  # 3. 变量标签修改
  # ==============================================
  if (!is.null(relabel)) {
    if (is.null(names(relabel)) || any(names(relabel) == "")) {
      warning(paste0(icon_error, " relabel必须是命名向量（名称为变量名，值为标签）"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " relabel格式错误，跳过"))
    } else {
      valid_vars <- intersect(names(relabel), final_var_names)
      if (length(valid_vars) > 0) {
        label_mapping$label[match(valid_vars, final_var_names)] <- relabel[valid_vars]
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 完成标签修改：", length(valid_vars), "个变量"))
      } else {
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 无有效标签修改变量"))
      }
    }
  }

  # ==============================================
  # 4. 类型转换（核心逻辑：R手动设置优先，Excel仅可选补充）
  # ==============================================
  # 收集R用户手动设置的类型转换
  type_conversion <- list(
    factor = if (!is.null(retype.factor)) setNames(rep("factor", length(retype.factor)), retype.factor) else NULL,
    numeric = if (!is.null(retype.numeric)) setNames(rep("numeric", length(retype.numeric)), retype.numeric) else NULL,
    character = if (!is.null(retype.character)) setNames(rep("character", length(retype.character)), retype.character) else NULL,
    integer = if (!is.null(retype.integer)) setNames(rep("integer", length(retype.integer)), retype.integer) else NULL,
    Date = if (!is.null(retype.Date)) setNames(rep("Date", length(retype.Date)), retype.Date) else NULL,
    logical = if (!is.null(retype.logical)) setNames(rep("logical", length(retype.logical)), retype.logical) else NULL
  )

  # 仅当excel_inherit_types=TRUE时，补充Excel中的类型规则（不覆盖手动设置）
  if (excel_inherit_types && !is.null(excel_super)) {
    # 补充Excel因子变量（排除已手动设置的）
    excel_factor_add <- setdiff(excel_types$factor, names(type_conversion$factor))
    if (length(excel_factor_add) > 0) {
      type_conversion$factor <- c(
        type_conversion$factor,
        setNames(rep("factor", length(excel_factor_add)), excel_factor_add)
      )
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 从Excel补充因子变量：", paste(excel_factor_add, collapse = ",")))
    }

    # 补充Excel数值变量（排除已手动设置的）
    excel_numeric_add <- setdiff(excel_types$numeric, names(type_conversion$numeric))
    if (length(excel_numeric_add) > 0) {
      type_conversion$numeric <- c(
        type_conversion$numeric,
        setNames(rep("numeric", length(excel_numeric_add)), excel_numeric_add)
      )
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 从Excel补充数值变量：", paste(excel_numeric_add, collapse = ",")))
    }
  }

  # 执行类型转换（对retype.factor直接执行as.factor）
  # 修复：移除变量名中的类型前缀
  retype <- unlist(type_conversion)

  # 新增：检查retype是否为NULL，只有非NULL时才设置名字
  if (!is.null(retype)) {
    # 清除变量名中的类型前缀（如"factor."、"numeric."等）
    names(retype) <- sub("^[^.]+\\.", "", names(retype))
  }

  if (length(retype) > 0) {
    success_conv <- list(factor=0, numeric=0, character=0, integer=0, Date=0, logical=0)
    valid_vars <- intersect(names(retype), final_var_names)
    invalid_vars <- setdiff(names(retype), final_var_names)

    if (length(invalid_vars) > 0) {
      cleaning_log <- c(cleaning_log, paste0(icon_warning, " 跳过不存在的转换变量：", paste(invalid_vars, collapse = ",")))
    }

    # 逐个变量执行转换
    for (var in valid_vars) {
      type <- retype[var]
      original_type <- class(df[[var]])[1]

      # 核心：直接转换（因子转换仅用as.factor）
      converted <- switch(
        type,
        factor = as.factor(df[[var]]),
        numeric = as.numeric(df[[var]]),
        integer = as.integer(df[[var]]),
        character = as.character(df[[var]]),
        Date = as.Date(df[[var]]),
        logical = as.logical(df[[var]]),
        NULL  # 无效类型返回NULL
      )

      if (!is.null(converted)) {
        df[[var]] <- converted
        success_conv[[type]] <- success_conv[[type]] + 1
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量'", var, "'类型转换：", original_type, "→", class(df[[var]])[1]))
      } else {
        cleaning_log <- c(cleaning_log, paste0(icon_error, " 变量'", var, "'转换失败（无效类型）"))
      }
    }

    # 转换统计汇总
    conv_summary <- paste(names(success_conv)[success_conv > 0],
                          success_conv[success_conv > 0], sep = ":", collapse = ", ")
    cleaning_log <- c(cleaning_log, paste0(icon_success, " 类型转换完成：共", sum(unlist(success_conv)), "个变量（", conv_summary, "）"))
  }

  # ==============================================
  # 5. 因子水平设置
  # ==============================================
  if (!is.null(factor_levels) && is.list(factor_levels) && !is.null(names(factor_levels))) {
    valid_factor_vars <- intersect(names(factor_levels), final_var_names)
    invalid_factor_vars <- setdiff(names(factor_levels), final_var_names)

    if (length(invalid_factor_vars) > 0) {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的因子变量：", paste(invalid_factor_vars, collapse = ",")))
    }

    success_factor <- 0
    for (var in valid_factor_vars) {
      # 确保变量为因子类型（否则尝试转换）
      if (!inherits(df[[var]], "factor")) {
        cleaning_log <- c(cleaning_log, paste0(icon_warning, " 变量'", var, "'不是因子，尝试自动转换"))
        df[[var]] <- as.factor(df[[var]])
        if (!inherits(df[[var]], "factor")) {
          cleaning_log <- c(cleaning_log, paste0(icon_error, " 变量'", var, "'无法转换为因子，跳过水平设置"))
          next
        }
      }

      params <- factor_levels[[var]]
      if (!is.list(params)) {
        cleaning_log <- c(cleaning_log, paste0(icon_warning, " 变量'", var, "'的因子配置不是列表，跳过"))
        next
      }

      # 提取水平和标签
      levels <- if (!is.null(params$levels)) params$levels else levels(df[[var]])
      labels <- if (!is.null(params$labels)) params$labels else levels
      ordered <- if (!is.null(params$ordered)) params$ordered else FALSE

      if (length(levels) != length(labels)) {
        cleaning_log <- c(cleaning_log, paste0(icon_warning, " 变量'", var, "'的levels与labels长度不匹配，使用默认水平"))
        levels <- levels(df[[var]])
        labels <- levels
      }

      # 设置因子水平
      df[[var]] <- factor(df[[var]], levels = levels, labels = labels, ordered = ordered)

      # 设置参照水平
      if (!is.null(params$ref_levels) && params$ref_levels %in% levels(df[[var]])) {
        df[[var]] <- relevel(df[[var]], ref = params$ref_levels)
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 变量'", var, "'参照水平设置为：", params$ref_levels))
      }

      success_factor <- success_factor + 1
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量'", var, "'因子水平设置完成"))
    }
    cleaning_log <- c(cleaning_log, paste0(icon_success, " 因子水平设置完成：共", success_factor, "个变量"))
  }

  # ==============================================
  # 6. 变量筛选（删除/保留）
  # ==============================================
  # 处理变量删除
  if (!is.null(drop_cols)) {
    drop_exist <- intersect(drop_cols, final_var_names)
    if (length(drop_exist) > 0) {
      df <- df[, setdiff(final_var_names, drop_exist), drop = FALSE]
      label_mapping <- label_mapping[-match(drop_exist, final_var_names), , drop = FALSE]
      final_var_names <- setdiff(final_var_names, drop_exist)
      original_order <- original_order[original_order %in% final_var_names]
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量删除完成：", length(drop_exist), "个变量"))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 无匹配的删除变量"))
    }
  }

  # 处理变量保留（优先级高于删除）
  if (!is.null(keep_cols)) {
    keep_exist <- intersect(keep_cols, final_var_names)
    if (length(keep_exist) > 0) {
      df <- df[, keep_exist, drop = FALSE]
      label_mapping <- label_mapping[match(keep_exist, final_var_names), , drop = FALSE]
      final_var_names <- keep_exist
      original_order <- original_order[original_order %in% final_var_names]
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量保留完成：", length(keep_exist), "个变量"))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 无匹配的保留变量"))
    }
  }

  # ==============================================
  # 7. 生成变量信息表
  # ==============================================
  if (ncol(df) == 0) {
    warning(paste0(icon_warning, " 所有变量已被删除，无法生成变量信息表"))
    viewer_table <- data.frame()
    var_stats <- list()
    var_types <- list()
  } else {
    # 变量信息提取函数
    get_var_info <- function(x, var_name) {
      base_type <- class(x)[1]
      inherit_type <- paste(class(x), collapse = "/")
      vals <- unique(x[!is.na(x)])
      n_vals <- length(vals)

      # 变量值展示逻辑
      val_show <- if (inherits(x, c("factor", "character"))) {
        freq <- table(x, useNA = "no")
        level_str <- paste0(names(freq), "(", freq, ")")
        if (length(level_str) > max_level) {
          paste0("共", length(level_str), "个水平：", paste(head(level_str, max_level), collapse = ","), "...")
        } else {
          paste(level_str, collapse = ",")
        }
      } else if (inherits(x, c("numeric", "integer"))) {
        paste0("范围: [", min(x, na.rm = TRUE), ", ", max(x, na.rm = TRUE), "]")
      } else {
        paste(head(vals, max_level), collapse = ",")
      }

      if (nchar(val_show) > max_level_pos) {
        val_show <- paste0(substr(val_show, 1, max_level_pos), "...")
      }

      list(
        基础类型 = base_type,
        继承类型 = inherit_type,
        变量值展示 = val_show
      )
    }

    # 提取所有变量的信息
    var_info_list <- lapply(seq_along(df), function(i) {
      get_var_info(df[[i]], names(df)[i])
    })
    names(var_info_list) <- names(df)

    # 整理变量类型信息
    var_types <- list(
      基础类型 = sapply(var_info_list, function(x) x$基础类型),
      继承类型 = sapply(var_info_list, function(x) x$继承类型),
      变量值展示 = sapply(var_info_list, function(x) x$变量值展示)
    )

    # 整理变量统计信息
    var_stats <- list(
      总观测数 = sapply(df, function(x) length(x)),
      缺失值数量 = sapply(df, function(x) sum(is.na(x))),
      有效值数量 = sapply(df, function(x) sum(!is.na(x))),
      变量值数量 = sapply(df, function(x) {
        if (inherits(x, "factor")) nlevels(x) else length(unique(x[!is.na(x)]))
      })
    )

    # 处理领域分类
    domain_values <- character(length(final_var_names))
    domain_rank <- integer(length(final_var_names))
    names(domain_values) <- final_var_names
    names(domain_rank) <- final_var_names

    # 新增：优先使用Excel中的dim和ID_NUM
    if (!is.null(excel_domain_mapping)) {
      # 遍历每个变量，从Excel映射中获取domain和domain_rank
      for (var in final_var_names) {
        if (var %in% excel_domain_mapping$variable) {
          idx <- which(excel_domain_mapping$variable == var)
          domain_values[var] <- as.character(excel_domain_mapping$dim[idx])
          domain_rank[var] <- as.integer(excel_domain_mapping$ID_NUM[idx])
        } else {
          domain_values[var] <- "未分类"
          domain_rank[var] <- NA
        }
      }
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 从Excel加载domain和domain_rank信息"))
    } else if (!is.null(domain_mark) && is.list(domain_mark)) {
      # 原有的domain_mark处理逻辑
      domain_seq <- names(domain_mark)
      all_vars_in_domain <- unlist(domain_mark)

      for (var in final_var_names) {
        if (!is.null(other_cols) && var %in% other_cols) {
          domain_values[var] <- "其他"
          domain_rank[var] <- NA
        } else {
          found <- FALSE
          for (domain_name in domain_seq) {
            if (var %in% domain_mark[[domain_name]]) {
              domain_values[var] <- domain_name
              domain_rank[var] <- which(domain_mark[[domain_name]] == var)
              found <- TRUE
              break
            }
          }
          if (!found) {
            domain_values[var] <- "未分类"
            domain_rank[var] <- NA
          }
        }
      }
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量分类完成：",
                                             length(all_vars_in_domain), "个变量分到", length(domain_mark), "个领域"))
    } else {
      domain_values[] <- NA
      domain_rank[] <- NA
      if (!is.null(domain_mark)) {
        warning(paste0(icon_warning, " domain_mark格式错误（应为列表），使用默认值"))
      }
    }

    # 生成最终信息表
    viewer_table <- data.frame(
      最终变量名 = final_var_names,
      变量名标签 = label_mapping$label[match(final_var_names, final_var_names)],
      基础类型 = var_types$基础类型[final_var_names],
      继承类型 = var_types$继承类型[final_var_names],
      `有效/缺失` = paste0(var_stats$有效值数量[final_var_names], " / ", var_stats$缺失值数量[final_var_names]),
      变量值 = var_stats$变量值数量[final_var_names],
      变量值域 = var_types$变量值展示[final_var_names],
      domain = domain_values[final_var_names],
      domain_rank = domain_rank[final_var_names],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # 新增：合并Excel中的其他列
    if (!is.null(excel_additional_cols)) {
      # 确保变量名匹配
      matched_vars <- intersect(excel_additional_cols$variable, final_var_names)
      if (length(matched_vars) > 0) {
        # 只保留匹配的变量
        excel_additional_sub <- excel_additional_cols[excel_additional_cols$variable %in% matched_vars, , drop = FALSE]

        # 按最终变量名排序
        excel_additional_sub <- excel_additional_sub[match(final_var_names, excel_additional_sub$variable), , drop = FALSE]

        # 移除variable列（已在viewer_table中）
        excel_additional_sub <- excel_additional_sub[, setdiff(colnames(excel_additional_sub), "variable"), drop = FALSE]

        # 处理列名冲突
        existing_cols <- colnames(viewer_table)
        new_cols <- colnames(excel_additional_sub)
        resolved_cols <- sapply(new_cols, function(col) {
          resolve_name_conflict(col, existing_cols)
        })
        colnames(excel_additional_sub) <- resolved_cols

        # 合并到viewer_table
        viewer_table <- cbind(viewer_table, excel_additional_sub)
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 合并Excel额外列：",
                                               paste(resolved_cols, collapse = ",")))
      }
    }
  }

  # ==============================================
  # 8. 处理add_mark参数（保持原始逻辑）
  # ==============================================
  if (!is.null(add_mark) && ncol(df) > 0) {
    if (exists("Tj_add_columns")) {
      current_vars <- final_var_names
      total_vars <- length(current_vars)

      if ("dim" %in% names(add_mark)) {
        existing_dims <- add_mark$dim
        full_dims <- sapply(current_vars, function(var) {
          if (var %in% names(existing_dims)) existing_dims[[var]] else default_mark
        })

        if (length(full_dims) != total_vars) {
          warning(paste0(icon_error, " dim长度异常,强制使用默认值"))
          full_dims <- rep(default_mark, total_vars)
          names(full_dims) <- current_vars
        }

        add_mark$dim <- full_dims
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 补全标记列：", total_vars, "个变量"))
      }

      temp_table <- Tj_add_columns(data = viewer_table, add = add_mark)
      new_cols <- setdiff(colnames(temp_table), colnames(viewer_table))
      existing_cols <- colnames(viewer_table)
      resolved_cols <- character(length(new_cols))

      for (i in seq_along(new_cols)) {
        col <- new_cols[i]
        resolved_col <- resolve_name_conflict(col, existing_cols)
        if (resolved_col != col) {
          warning(paste0(icon_warning, " 列名冲突: '", col, "' → '", resolved_col, "'"))
          cleaning_log <- c(cleaning_log, paste0(icon_warning, " 列名冲突: '", col, "'重命名为'", resolved_col, "'"))
        }
        resolved_cols[i] <- resolved_col
        existing_cols <- c(existing_cols, resolved_col)
      }

      colnames(temp_table)[match(new_cols, colnames(temp_table))] <- resolved_cols
      viewer_table <- temp_table

    } else {
      warning(paste0(icon_warning, " 未找到Tj_add_columns函数,无法处理add_mark"))
      cleaning_log <- c(cleaning_log, paste0(icon_warning, " 未找到Tj_add_columns,跳过add_mark"))
    }
  }

  # ==============================================
  # 9. 表格排序与展示（完全保留原始代码）
  # ==============================================
  sorted_vars <- if (!dynamic_view_mark) original_order else final_var_names

  if (!is.null(sort_mark) && ncol(viewer_table) > 0) {
    valid_sort_cols <- intersect(sort_mark, colnames(viewer_table))
    invalid_sort_cols <- setdiff(sort_mark, colnames(viewer_table))

    if (length(invalid_sort_cols) > 0) {
      warning(paste0(icon_warning, " 无效排序列：", paste(invalid_sort_cols, collapse = ",")))
      cleaning_log <- c(cleaning_log, paste0(icon_warning, " 无效排序列：", paste(invalid_sort_cols, collapse = ",")))
    }

    if (length(valid_sort_cols) > 0) {
      sort_expr <- lapply(valid_sort_cols, function(col) viewer_table[[col]])
      sorted_indices <- do.call(order, sort_expr)
      viewer_table <- viewer_table[sorted_indices, , drop = FALSE]
      sorted_vars <- viewer_table$最终变量名
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 表格排序完成：按", paste(valid_sort_cols, collapse = ",")))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 使用默认顺序展示表格"))
    }
  } else if (dynamic_view_mark && ncol(viewer_table) > 0) {
    valid_other_vars <- if (!is.null(other_cols)) intersect(other_cols, final_var_names) else character(0)

    modified_vars <- unique(c(
      if (!is.null(retype.factor)) retype.factor,
      if (!is.null(retype.numeric)) retype.numeric,
      if (!is.null(retype.character)) retype.character,
      if (!is.null(retype.integer)) retype.integer,
      if (!is.null(retype.Date)) retype.Date,
      if (!is.null(retype.logical)) retype.logical,
      if (!is.null(factor_levels)) names(factor_levels),
      if (!is.null(relabel)) names(relabel),
      if (!is.null(rename)) unname(rename)
    ))
    modified_vars <- setdiff(modified_vars, valid_other_vars)
    other_vars <- setdiff(final_var_names, c(valid_other_vars, modified_vars))
    sorted_vars <- c(valid_other_vars, modified_vars, other_vars)
    sorted_vars <- intersect(sorted_vars, final_var_names)
    viewer_table <- viewer_table[match(sorted_vars, viewer_table$最终变量名), , drop = FALSE]
  } else {
    viewer_table <- viewer_table[match(sorted_vars, viewer_table$最终变量名), , drop = FALSE]
  }

  # ==============================================
  # 10. 准备返回结果（包含原始表格展示逻辑）
  # ==============================================
  super_param <- list(
    data = df[, original_order, drop = FALSE],
    label_mapping = label_mapping,
    var_info = list(
      统计信息 = var_stats,
      类型信息 = var_types,
      关键变量 = if (!is.null(other_cols) && ncol(df) > 0) {
        setNames(lapply(other_cols, function(k) {
          if (k %in% final_var_names) list(存在 = TRUE) else list(存在 = FALSE)
        }), other_cols)
      } else NULL
    ),
    Viewer = list(table = viewer_table),
    cleaning_log = cleaning_log,
    stats = list(
      样本量 = nrow(df),
      变量数 = ncol(df),
      最终变量名类型 = rename_theme,
      初始变量数 = init_ncol,
      删除变量数 = init_ncol - ncol(df)
    )
  )

  # 输出清洗日志
  cat("\n===== 清洗日志汇总 =====\n")
  cat(paste(super_param$cleaning_log, collapse = "\n"))

  # 显示变量信息表（完全保留原始展示代码，未做任何修改）
  if (ncol(df) > 0 && requireNamespace("flextable", quietly = TRUE)) {
    fftable <- viewer_table
    ft <- flextable(fftable) |>
      theme_box() |>
      set_caption(caption = "Table. Variable information") |>
      hline_top(part = "all", border = officer::fp_border(color = "black", width = 1.25)) |>
      hline_bottom(part = "all", border = officer::fp_border(color = "black", width = 1.25)) |>
      autofit() |>
      bold(bold = TRUE, part = c("all")) |>
      italic(italic = TRUE, part = "header") |>
      align(align = "center", part = "all")

    # 应用默认背景色
    ft <- ft |>
      bg(i = ~ 继承类型 == "character", bg = "#E3F2FD", part = "body") |>
      bg(i = ~ 继承类型 == "numeric", bg = "#D9D9D5", part = "body") |>
      bg(i = ~ 继承类型 == "factor", bg = "#FFE8CC", part = "body")

    # 为domain列添加颜色区分
    if ("domain" %in% colnames(fftable) && !all(is.na(fftable$domain))) {
      domains <- unique(fftable$domain[!is.na(fftable$domain)])
      domain_colors <- get_distinct_colors(length(domains))
      names(domain_colors) <- domains

      for (d in domains) {
        formula_str <- paste0("~ domain == '", gsub("'", "\\'", d), "'")
        row_formula <- as.formula(formula_str)
        ft <- ft |> bg(i = row_formula, j = "domain", bg = domain_colors[d], part = "body")
      }
    }

    # 应用color_col_mark着色
    if (!is.null(color_col_mark)) {
      color_cols <- if (is.character(color_col_mark)) color_col_mark else as.character(color_col_mark)
      valid_cols <- intersect(color_cols, colnames(fftable))
      invalid_cols <- setdiff(color_cols, colnames(fftable))

      if (length(invalid_cols) > 0) {
        warning(paste0(icon_warning, " 无效着色列：", paste(invalid_cols, collapse = ",")))
      }

      if (length(valid_cols) > 0) {
        for (col in valid_cols) {
          levels <- unique(fftable[[col]])
          levels <- levels[!is.na(levels)]
          n_levels <- length(levels)
          if (n_levels == 0) next

          colors <- get_distinct_colors(n_levels)
          names(colors) <- levels
          cleaning_log <- c(cleaning_log, paste0(icon_info, " 为列'", col, "'分配", n_levels, "种颜色"))

          for (level in levels) {
            row_formula_str <- paste0("~ `", gsub("`", "\\`", col), "` == '", gsub("'", "\\'", level), "'")
            col_formula <- as.formula(paste0("~ `", gsub("`", "\\`", col), "`"))
            ft <- ft |> bg(i = as.formula(row_formula_str), j = col_formula, bg = colors[level], part = "body")
          }
        }
      }
    }

    print(ft)
  } else if (ncol(df) > 0) {
    message(paste0(icon_info, " 安装flextable查看格式化表格(install.packages('flextable'))"))
    print(super_param$Viewer$table)
  }
  return(super_param)
}
