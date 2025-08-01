#' 数据变量综合处理与信息管理工具
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
#' @param retype 命名向量，可选参数。用于指定变量类型转换规则，格式为`c("最终变量名"="目标类型")`。
#'   支持的目标类型包括："numeric"、"integer"、"factor"、"ordered"、"Date"、"character"、"logical"。
#'   若指定的变量不存在，将在日志中提示警告；字符型强行转化为因子或数值变量时，会根据字符的unicode编码
#'   对水平进行排序并从1开始计数因子水平。（替代原`type_conv`参数）
#' @param factor_levels 列表，可选参数。用于自定义因子变量的水平设置，格式为`list("最终变量名"=list(levels=, labels=, ordered=, ref_levels=))`。
#'   其中：
#'   - levels：必须项，指定水平顺序；
#'   - labels：可选项，水平标签（默认与levels一致）；
#'   - ordered：可选项，是否为有序因子（默认FALSE）；
#'   - ref_levels：可选项，指定参照水平（支持通过水平值或标签设置）。
#'   辅助函数：
#'   - "Tj_to_factor_list2"可将相关数据文件转化为可输入的列表；
#'   - "fast_factor_same"可快速声明多个相同因子赋值的变量（详见对应函数帮助）。
#' @param drop_cols 字符向量，可选参数。指定需要从数据中删除的变量名称（基于最终变量名），
#'   格式为`c("最终变量名1", "最终变量名2")`。删除操作在类型转换和因子设置之后执行。
#'   若与`keep_cols`同时设置，`keep_cols`优先。
#' @param keep_cols 字符向量，可选参数。指定需要从数据中保留的变量名称（基于最终变量名），
#'   格式为`c("最终变量名1", "最终变量名2")`。保留操作在类型转换和因子设置之后执行，
#'   优先级高于`drop_cols`。
#' @param other_vars 字符向量，可选参数。标记为关键变量的名称（基于`最终变量名`），在动态排序中
#'   关键变量将优先展示在结果的最前面，可用于区分分析中需重点关注的变量。（替代原`key_vars`参数）
#' @param rename 命名向量，可选参数。用于自定义变量重命名规则，格式为`c("旧最终变量名"="新最终变量名")`。
#'   该操作将覆盖由`rename_safe`参数设置的初始最终变量名，重命名失败的变量会在日志中提示。
#' @param rename_safe 字符向量，可选参数。指定最终变量名的初始格式，决定变量名的基础样式，各可选值及处理规则如下：
#'   "normal_name"：保留中文及合法字符（a-zA-Z0-9_），将所有非法符号（如空格、标点、特殊字符等）转换为下划线；若多个非法符号相邻，仅保留一个下划线；最终变量名通过`make.unique`确保唯一性。
#'   "unicode_name"：将中文及所有非法字符转换为Unicode编码（格式为"u+十六进制值"），仅保留a-zA-Z0-9_等合法字符；最终变量名通过`make.unique`确保唯一性，适合需跨系统兼容的场景。
#'   "original_name"：完全保留原始变量名，不做任何转换；若包含非法符号（如空格、特殊字符等），可能导致后续模型运行时自动重命名或报错。
#'   "label"：使用变量的标签（label）作为初始变量名，若标签不存在则默认使用原始变量名。
#'   默认值为`c("unicode_name", "normal_name", "original_name", "label")`，**建议优先使用"normal_name"**，兼顾可读性与兼容性。
#' @param relabel 命名向量，可选参数。用于修改变量的标签信息，格式为`c("最终变量名"="新label")`。
#'   标签主要用于变量信息表中的展示，不影响变量实际名称和数据处理。
#' @param add_mark 列表，可选参数。用于在变量信息汇总表中添加自定义标记列，
#'   格式为`list(<标记列名1>=<标记规则1>, <标记列名2>=<标记规则2>)`，其中标记规则支持tidy的mutate语法。
#'   若标记列包含"dim"，未匹配的变量将使用`default_mark`填充。
#' @param dynamic_sort 逻辑型，可选参数。是否启用变量动态排序功能。若为TRUE（默认），
#'   变量顺序为"关键变量(other_vars) > 经过修改的变量 > 其他变量"；若为FALSE，保持原始变量顺序。
#'   若`sort_mark`已设置，则`dynamic_sort`自动失效。
#' @param max_level 整数，可选参数。控制变量值展示的最大水平数量，超过该数量时仅显示前N个水平
#'   并提示总数，默认值为5。
#' @param color_col_mark 字符向量，可选参数。指定需要进行颜色标记的列名（基于变量信息汇总表），
#'   函数会为该列的不同水平自动分配区分度良好的颜色，增强表格可读性。
#' @param max_level_pos 整数，可选参数。控制变量值展示的最大字符长度，超过该长度将截断并添加"..."，
#'   默认值为30。
#' @param excel_super 列表，可选参数。用于导入Excel格式的批量处理配置表，格式为`list(var=..., format=...)`，其中：
#'   - var：包含"最终变量名"、"variable"（新变量名）、"label"、"cont"（类型标识：1=数值型，0=因子型）等列的数据框；
#'   - format：包含因子水平配置的辅助数据框。
#'   若设置此参数，将自动解析配置表并覆盖`rename`、`relabel`、`retype`、`factor_levels`等参数的手动设置，
#'   适合大规模变量的标准化处理。
#' @param default_mark 可选参数。当`add_mark`中包含"dim"列且部分变量未匹配到标记规则时，
#'   使用此值作为默认填充，默认值为NA。
#' @param sort_mark 字符向量，可选参数。指定用于排序的变量名（基于处理后的数据集），
#'   变量信息表将按指定变量的取值排序，优先级高于`dynamic_sort`。若指定变量不存在，将在日志中警告并使用默认排序。
#'
#' @details
#' 变量处理优先级：\cr
#' 1. 批量配置解析（`excel_super`）→ 2. 初始变量名生成（`rename_safe`）→ 3. 自定义重命名（`rename`）→ \cr
#' 4. 标签修改（`relabel`）→ 5. 类型转换（`retype`）→ 6. 因子水平设置（`factor_levels`）→ \cr
#' 7. 变量筛选（`keep_cols`优先于`drop_cols`）→ 8. 排序（`sort_mark`优先于`dynamic_sort`）。\cr
#'
#' 表格着色逻辑：通过`color_col_mark`指定列后，函数会为该列的每个唯一水平分配不同的色调，
#' 增强不同类别间的视觉区分度，未指定时默认按变量类型（字符型/数值型/因子型）着色。
#'
#' 日志信息包含：各步骤操作结果、警告信息、错误原因等，便于追踪处理过程和排查问题。
#'
#' @return
#' 列表，包含以下核心组件：
#' \code{data}: 处理后的标准化数据集，变量按指定顺序排列；
#' \code{label_mapping}: 变量名映射表，包含original_name、normal_name、unicode_name、label等信息；
#' \code{var_info}: 变量信息列表，包括统计信息（总观测数、缺失值数量等）和类型信息（基础类型、继承类型等）；
#' \code{Viewer}: 包含格式化的变量信息汇总表，展示变量的各类属性和统计特征，支持颜色标记；
#' \code{cleaning_log}: 清洗过程的详细日志，记录每一步操作的结果和警告信息；
#' \code{stats}: 数据处理前后的统计对比，包括样本量、变量数、删除变量数等。
#'
#' @note
#' 建议在处理大型数据集前先进行测试，以评估函数性能；\cr
#' 变量名若包含特殊字符，会被自动转换为安全格式，转换规则可通过`rename_safe`参数控制；\cr
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
#' library(Tjpackages)
#' data("df")  # 假设df为待处理的原始数据集
#'
#' # 示例1：基础使用，查看数据处理结果
#' dat.param1 <- super_param(data = df)
#' # 查看处理后的数据和变量信息表
#' head(dat.param1$data)
#' dat.param1$Viewer$table
#'
#' # 示例2：指定变量名规则并标记关键变量
#' dat.param2 <- super_param(
#'   data = df,
#'   rename_safe = "normal_name",  # 生成保留中文和合法字符的变量名
#'   other_vars = c("ID", "性别")  # 标记关键变量，优先展示
#' )
#'
#' # 示例3：自定义重命名、标签与类型转换
#' dat.param3 <- super_param(
#'   data = df,
#'   rename_safe = "normal_name",
#'   other_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),  # 自定义重命名
#'   relabel = c(yx = "亚型", 年龄 = "年龄(岁)"),  # 修改变量标签
#'   retype = c(年龄 = "numeric", yx = "factor")  # 类型转换
#' )
#'
#' # 示例4：设置因子水平与参照组
#' dat.param4 <- super_param(
#'   data = df,
#'   rename_safe = "normal_name",
#'   other_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   relabel = c(yx = "亚型", 年龄 = "年龄(岁)"),
#'   retype = c(年龄 = "numeric", yx = "factor", Gender = "factor"),
#'   # 自定义因子水平与参照组
#'   factor_levels = list(
#'     y = list(levels = c(1, 2), labels = c("存活", "死亡"), ref_levels = "存活"),
#'     yx = list(levels = 1:5, labels = paste0("亚型", 1:5), ordered = TRUE),
#'     Gender = list(levels = c(1, 2), labels = c("男", "女"), ref_levels = "男")
#'   )
#' )
#'
#' # 示例5：变量筛选与排序
#' dat.param5 <- super_param(
#'   data = df,
#'   rename_safe = "normal_name",
#'   keep_cols = c("ID", "年龄", "yx", "Gender"),  # 仅保留指定变量
#'   sort_mark = "年龄"  # 按年龄排序
#' )
#'
#' # 示例6：添加自定义标记列并启用颜色标记
#' dat.param6 <- super_param(
#'   data = df,
#'   rename_safe = "normal_name",
#'   other_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   add_mark = list(
#'     数据类型 = "ifelse(基础类型 %in% c('integer','numeric'), '连续型', '分类型')",
#'     重要性 = "dplyr::case_when(最终变量名 %in% c('ID','年龄') ~ '高', TRUE ~ '中')"
#'   ),
#'   color_col_mark = "数据类型"  # 按数据类型着色
#' )
#'
#' # 示例7：通过Excel配置表批量处理
#' # 假设已读取Excel配置表为list(var=Super_Single, format=add_label_rename_format)
#' if (exists("excel_config")) {  # excel_config为导入的配置表列表
#'   dat.param7 <- super_param(
#'     data = df,
#'     rename_safe = "normal_name",
#'     excel_super = excel_config,  # 批量配置优先于手动设置
#'     color_col_mark = "dim"  # 按dim列着色
#'   )
#' }
#'
#' # 保存处理结果
#' data.lib <- list(raw = df, processed = dat.param6)
#' save(data.lib, file = "Super_param_result.rda")
#'
#' @export
super_paramtest <- function(data,
                            retype = NULL,
                            factor_levels = NULL,
                            drop_cols = NULL,
                            keep_cols = NULL,
                            other_vars = NULL,
                            rename = NULL,
                            rename_safe = c("unicode_name", "normal_name", "original_name", "label"),
                            relabel = NULL,
                            add_mark = NULL,
                            dynamic_sort = TRUE,
                            max_level = 5,
                            color_col_mark = NULL,
                            max_level_pos = 30,
                            excel_super = NULL,
                            default_mark = NA,
                            sort_mark = NULL  # 新增：用于对Viewer表格排序的列名向量
) {
  # 定义Unicode图标
  icon_info <- "\U27A1\UFE0F"    #
  icon_success <- "\u2705" #
  icon_warning <- "\U26A0\UFE0F" #
  icon_error <- "\U274C"   #

  # 核心：定义Viewer表格的原始必要列(固定列名）
  core_cols <- c("最终变量名", "label", "基础类型", "继承类型",
                 "总(有效/缺失)", "水平数_唯一值", "变量值水平")

  # 检查必要的包
  if (!requireNamespace("flextable", quietly = TRUE)) {
    warning(paste0(icon_warning, " flextable包未安装,表格可视化功能将受限"))
  }

  # 检查输入数据是否为数据框
  if (!inherits(data, "data.frame")) {
    warning(paste0(icon_error, " 输入数据必须为数据框,返回NULL"))
    return(NULL)
  }

  # 记录初始日志
  cleaning_log <- character(0)
  cleaning_log <- c(cleaning_log, paste0(icon_info, " 执行日期:", Sys.time()))

  # 初始化变量
  df <- data
  init_raw_vars <- names(df)
  cleaning_log <- c(cleaning_log, paste0(icon_info, " 原始数据包含变量：", paste(init_raw_vars, collapse = ",")))

  # 处理excel_super参数(核心矫正逻辑）
  Super_Single <- NULL
  if (!is.null(excel_super)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 检测到excel_super参数,开始优先处理"))
    if (!all(c("format", "var") %in% names(excel_super))) {
      warning(paste0(icon_error, " excel_super参数格式错误,需包含'format'和'var'元素"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " 警告：excel_super格式错误,跳过处理"))
    } else {
      Super_Single <- excel_super$var
      add_label_rename_format <- excel_super$format
      required_cols <- c("最终变量名", "variable", "label", "cont")
      missing_cols <- setdiff(required_cols, colnames(Super_Single))
      if (length(missing_cols) > 0) {
        warning(paste0(icon_warning, " Super_Single缺少必要列：", paste(missing_cols, collapse = ",")))
        cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：Super_Single缺少列：", paste(missing_cols, collapse = ",")))
      } else {
        # 变量匹配与矫正
        valid_vars <- intersect(Super_Single$最终变量名, init_raw_vars)
        missing_vars <- setdiff(Super_Single$最终变量名, init_raw_vars)
        if (length(missing_vars) > 0) {
          cleaning_log <- c(cleaning_log, paste0(icon_info, " 注意：以下变量在数据中不存在,将跳过处理：", paste(missing_vars, collapse = ",")))
        }

        # 参数矫正(重命名、标签、类型）
        Super_Single_valid <- Super_Single[Super_Single$最终变量名 %in% valid_vars, , drop = FALSE]
        rename <- setNames(Super_Single_valid$variable, Super_Single_valid$最终变量名)
        relabel <- setNames(Super_Single_valid$label, Super_Single_valid$最终变量名)

        # 类型转换映射矫正
        retype_mapping <- c("1" = "numeric", "0" = "factor")
        retype_raw <- as.character(Super_Single_valid$cont)
        retype <- setNames(retype_mapping[retype_raw], Super_Single_valid$最终变量名)
        unrecognized_types <- unique(retype_raw[is.na(retype_mapping[retype_raw])])
        if (length(unrecognized_types) > 0) {
          cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：未识别的类型值", paste(unrecognized_types, collapse = ","), ",已跳过"))
          retype <- retype[!is.na(retype)]
        }

        # 因子水平矫正
        if (exists("Tj_factor_list")) {
          factor_levels_all <- Tj_factor_list(add_label_rename_format)
          factor_levels <- factor_levels_all[names(factor_levels_all) %in% valid_vars]
          cleaning_log <- c(cleaning_log, paste0(icon_info, " 从Tj_factor_list获取factor_levels,共保留", length(factor_levels), "个有效变量的配置"))
        } else {
          warning(paste0(icon_warning, " 未找到Tj_factor_list函数,无法设置factor_levels"))
          cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：未找到Tj_factor_list函数,跳过factor_levels设置"))
        }

        # add_mark矫正
        if ("dim" %in% colnames(Super_Single_valid)) {
          add_mark <- list(dim = setNames(Super_Single_valid$dim, Super_Single_valid$最终变量名))
        } else {
          add_mark <- NULL
          cleaning_log <- c(cleaning_log, paste0(icon_info, " Super_Single中未找到'dim'列,不设置add_mark"))
        }

        cleaning_log <- c(cleaning_log, paste0(icon_success, " excel_super参数处理完成,成功矫正", length(valid_vars), "个变量"))
      }
    }
  }

  # 处理keep_cols与drop_cols的冲突
  if (!is.null(keep_cols) && !is.null(drop_cols)) {
    warning(paste0(icon_warning, " keep_cols与drop_cols同时设置,将优先使用keep_cols"))
    cleaning_log <- c(cleaning_log, paste0(icon_info, " keep_cols与drop_cols同时设置,优先使用keep_cols"))
    drop_cols <- NULL
  }

  # 初始化变量
  init_nrow <- nrow(df)
  init_ncol <- ncol(df)

  # 安全执行函数
  safe_exec <- function(expr, msg) {
    tryCatch(
      expr,
      error = function(e) {
        warning(paste0(icon_error, " [清洗警告]", msg, "(错误：", e$message, ")"))
        cleaning_log <<- c(cleaning_log, paste0(icon_error, " [错误]", msg, "(原因：", e$message, ")"))
        return(NULL)
      }
    )
  }

  # 变量名转换辅助函数
  get_universal_name <- function(char, old_names) {
    char_list <- strsplit(char, "")[[1]]
    processed <- sapply(char_list, function(c) {
      if (grepl("[a-zA-Z0-9_]", c))
        c
      else
        paste0("u", as.hexmode(utf8ToInt(c)))
    })
    temp <- paste(processed, collapse = "")
    if (grepl("^[0-9]", temp))
      temp <- paste0("v", temp)
    if (nchar(temp) == 0)
      temp <- paste0("var_", which(old_names == char))
    temp
  }

  # 生成不同格式的变量名
  legal_chars <- "a-zA-Z0-9_\u4e00-\u9fa5"
  normal_name <- sapply(init_raw_vars, function(name) {
    new_name <- gsub(paste0("[^", legal_chars, "]"), "_", name)
    gsub("_+", "_", new_name)
  })
  normal_name <- make.unique(normal_name, sep = "_")

  unicode_name <- sapply(init_raw_vars, function(x)
    get_universal_name(x, init_raw_vars))
  unicode_name <- make.unique(unicode_name, sep = "_")

  # 创建标签映射表
  label_mapping <- data.frame(
    original_name = init_raw_vars,
    normal_name = normal_name,
    unicode_name = unicode_name,
    label = init_raw_vars,
    stringsAsFactors = FALSE
  )

  # 处理rename_safe
  rename_safe <- match.arg(rename_safe)
  number <- ncol(df)
  initial_final_names <- switch(
    rename_safe,
    "original_name" = label_mapping$original_name,
    "normal_name" = label_mapping$normal_name,
    "unicode_name" = label_mapping$unicode_name,
    "label" = label_mapping$label
  )
  names(df) <- initial_final_names
  cleaning_log <- c(cleaning_log,
                    paste0(icon_info, " 初始最终变量名(基于", rename_safe, ")：" , number, "个变量"))

  # 处理重命名
  if (!is.null(rename)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行自定义重命名..."))
    if (is.null(names(rename)) || any(names(rename) == "")) {
      warning(paste0(icon_error, " rename参数格式错误：必须为命名向量"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " 警告：rename格式错误,跳过"))
    } else {
      exist_vars <- names(rename) %in% names(df)
      if (any(!exist_vars)) {
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的重命名变量：", paste(names(rename)[!exist_vars], collapse = ",")))
        rename <- rename[exist_vars]
      }
      if (length(rename) > 0) {
        names(df)[match(names(rename), names(df))] <- rename
        label_mapping$最终变量名 <- names(df)
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 自定义重命名完成：", paste(paste0(names(rename), "→", rename), collapse = ",")))
      }
    }
  }
  final_var_names <- names(df)  # 当前所有变量

  # 处理标签修改
  if (!is.null(relabel)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行label修改..."))
    if (is.null(names(relabel)) || any(names(relabel) == "")) {
      warning(paste0(icon_error, " relabel参数格式错误：必须为命名向量"))
      cleaning_log <- c(cleaning_log, paste0(icon_error, " 警告：relabel格式错误,跳过"))
    } else {
      var_indices <- match(names(relabel), final_var_names)
      valid_indices <- which(!is.na(var_indices))
      invalid_vars <- names(relabel)[is.na(var_indices)]
      if (length(invalid_vars) > 0) {
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的标签修改变量：", paste(invalid_vars, collapse = ",")))
      }
      if (length(valid_indices) > 0) {
        label_mapping$label[var_indices[valid_indices]] <- relabel[valid_indices]
        cleaning_log <- c(cleaning_log, paste0(icon_success, " 成功修改", length(valid_indices), "个变量的label"))
      }
    }
  }

  # 处理类型转换
  if (!is.null(retype)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行类型转换..."))
    success_conv <- character(0)
    valid_retype_vars <- intersect(names(retype), final_var_names)
    invalid_retype_vars <- setdiff(names(retype), final_var_names)
    if (length(invalid_retype_vars) > 0) {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的类型转换变量：", paste(invalid_retype_vars, collapse = ",")))
    }
    for (var in valid_retype_vars) {
      type <- retype[var]
      if (!type %in% c("numeric", "integer", "factor", "ordered", "Date", "character", "logical")) {
        cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：不支持的类型", type, ",变量", var, "转换失败"))
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
      }, paste("变量", var, "转换为", type, "失败"))
      if (!is.null(converted)) {
        df[[var]] <- converted
        success_conv <- c(success_conv, var)
      }
    }
    cleaning_log <- c(cleaning_log, paste0(icon_success, " 类型转换完成,成功转换", length(success_conv), "个变量"))
  }

  # 处理因子水平设置
  if (!is.null(factor_levels)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行因子水平设置..."))
    success_factor <- character(0)
    valid_factor_vars <- intersect(names(factor_levels), final_var_names)
    invalid_factor_vars <- setdiff(names(factor_levels), final_var_names)
    if (length(invalid_factor_vars) > 0) {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 跳过不存在的因子设置变量：", paste(invalid_factor_vars, collapse = ",")))
    }
    for (var in valid_factor_vars) {
      params <- factor_levels[[var]]
      current_var <- df[[var]]
      ref_levels <- params$ref_levels
      ordered <- if (!is.null(params$ordered)) params$ordered else FALSE

      if (is.null(params$levels)) {
        if (inherits(current_var, "factor")) {
          levels <- levels(current_var)
          labels <- levels(current_var)
          cleaning_log <- c(cleaning_log, paste0(icon_info, " 变量", var, "已是因子,使用现有水平：", paste(levels, collapse = ",")))
        } else {
          levels <- unique(current_var[!is.na(current_var)])
          labels <- as.character(levels)
          cleaning_log <- c(cleaning_log, paste0(icon_info, " 变量", var, "非因子,自动生成水平：", paste(levels, collapse = ",")))
        }
      } else {
        levels <- params$levels
        labels <- if (!is.null(params$labels)) params$labels else levels
        if (length(levels) != length(labels)) {
          cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：变量", var, "的levels和labels长度不一致,使用默认labels"))
          labels <- levels
        }
      }

      new_factor <- safe_exec({
        factor(x = current_var, levels = levels, labels = labels, ordered = ordered)
      }, paste("变量", var, "因子设置失败"))

      if (!is.null(new_factor)) {
        if (!is.null(ref_levels)) {
          if (ref_levels %in% levels(new_factor)) {
            new_factor <- relevel(new_factor, ref = ref_levels)
            cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量", var, "成功设置参照组为水平值：", ref_levels))
          } else if (ref_levels %in% labels(new_factor)) {
            level_idx <- which(labels(new_factor) == ref_levels)
            if (length(level_idx) > 0) {
              ref_level_val <- levels(new_factor)[level_idx[1]]
              new_factor <- relevel(new_factor, ref = ref_level_val)
              cleaning_log <- c(cleaning_log, paste0(icon_success, " 变量", var, "成功设置参照组为标签：", ref_levels))
            } else {
              cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：变量", var, "的参照组", ref_levels, "不在标签中"))
            }
          } else {
            cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：变量", var, "的参照组", ref_levels, "不在因子水平中"))
          }
        }

        df[[var]] <- new_factor
        success_factor <- c(success_factor, var)
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 变量", var, "处理为", ifelse(ordered, "有序因子", "无序因子")))
      }
    }
    cleaning_log <- c(cleaning_log, paste0(icon_success, " 因子设置完成,成功处理", length(success_factor), "个变量"))
  }

  # 处理变量删除
  if (!is.null(drop_cols)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行变量删除..."))
    drop_exist <- intersect(drop_cols, final_var_names)
    if (length(drop_exist) > 0) {
      df <- df[, setdiff(final_var_names, drop_exist), drop = FALSE]
      label_mapping <- label_mapping[-match(drop_exist, final_var_names), , drop = FALSE]
      final_var_names <- final_var_names[-match(drop_exist, final_var_names)]
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 删除变量：", paste(drop_exist, collapse = ",")))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 无匹配的变量可删除"))
    }
  }

  # 处理变量保留
  if (!is.null(keep_cols)) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 基于最终变量名执行变量保留..."))
    keep_exist <- intersect(keep_cols, final_var_names)
    if (length(keep_exist) > 0) {
      df <- df[, keep_exist, drop = FALSE]
      label_mapping <- label_mapping[match(keep_exist, final_var_names), , drop = FALSE]
      final_var_names <- final_var_names[match(keep_exist, final_var_names)]
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 保留变量：", paste(keep_exist, collapse = ",")))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " 无匹配的变量可保留"))
    }
  }

  # 生成变量信息表(原始必要列）
  if (ncol(df) == 0) {
    warning(paste0(icon_warning, " 所有变量已被删除,无法生成变量信息表"))
    viewer_table <- data.frame()
    var_stats <- list()
    var_types <- list()
  } else {
    TJ_get_vartype <- function(x) {
      base_type <- class(x)[1]
      inherit_type <- paste(class(x), collapse = "/")
      vals <- unique(x[!is.na(x)])
      val_show <- if (length(vals) > max_level) {
        paste0("共", length(vals), "个水平(前", max_level, "个)：",
               paste(head(vals, max_level), collapse = ","))
      } else {
        paste(vals, collapse = ",")
      }
      if (nchar(val_show) > max_level_pos)
        val_show <- paste0(substr(val_show, 1, max_level_pos), "...")
      list(基础类型 = base_type,
           继承类型 = inherit_type,
           变量值展示 = val_show)
    }

    type_details <- lapply(df, TJ_get_vartype)
    var_types <- list(
      基础类型 = sapply(type_details, function(x) x$基础类型),
      继承类型 = sapply(type_details, function(x) x$继承类型),
      变量值展示 = sapply(type_details, function(x) x$变量值展示)
    )

    var_stats <- list(
      总观测数 = sapply(df, function(x) length(x)),
      缺失值数量 = sapply(df, function(x) sum(is.na(x))),
      有效值数量 = sapply(df, function(x) sum(!is.na(x))),
      水平数_唯一值 = sapply(df, function(x) {
        if (inherits(x, "factor")) nlevels(x)
        else length(unique(x[!is.na(x)]))
      })
    )

    current_original_names <- label_mapping$original_name
    merged_names <- with(label_mapping,
                         paste(original_name, normal_name, unicode_name, sep = "/"))
    names(merged_names) <- current_original_names
    merged_names_sorted <- sapply(final_var_names, function(var) {
      orig_idx <- match(var, final_var_names)
      if (!is.na(orig_idx)) {
        orig_name <- current_original_names[orig_idx]
        merged_names[orig_name]
      } else {
        NA
      }
    })

    # 严格按照核心必要列生成表格
    viewer_table <- data.frame(
      最终变量名 = final_var_names,
      label = label_mapping$label[match(final_var_names, final_var_names)],
      基础类型 = var_types$基础类型[final_var_names],
      继承类型 = var_types$继承类型[final_var_names],
      `总(有效/缺失)` = paste0(
        var_stats$总观测数[final_var_names],
        " (", var_stats$有效值数量[final_var_names],
        " / ", var_stats$缺失值数量[final_var_names], ")"
      ),
      水平数_唯一值 = var_stats$水平数_唯一值[final_var_names],
      变量值水平 = var_types$变量值展示[final_var_names],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  # 处理Super_Single合并(核心修复：避免与必要列重名）
  if (!is.null(Super_Single) && "最终变量名" %in% colnames(Super_Single) && ncol(df) > 0) {
    cleaning_log <- c(cleaning_log, paste0("\n", icon_info, " 开始匹配Super_Single与Viewer$table..."))

    # 1. 提取待合并列并检查与核心列的冲突
    super_all_vars <- Super_Single$最终变量名
    viewer_vars <- viewer_table$最终变量名
    total_super_rows <- length(super_all_vars)

    # 2. 筛选有效匹配的Super_Single行
    matched_vars <- intersect(super_all_vars, viewer_vars)
    unmatched_vars <- setdiff(super_all_vars, viewer_vars)
    matched_count <- length(matched_vars)
    unmatched_count <- length(unmatched_vars)

    # 3. 处理合并列名冲突(关键修复）
    if (matched_count > 0) {
      super_matched <- Super_Single[Super_Single$最终变量名 %in% matched_vars, , drop = FALSE]
      super_merge_cols <- setdiff(colnames(super_matched), "最终变量名")

      # 检查并处理与核心列重名的列(如label）
      conflict_cols <- intersect(super_merge_cols, core_cols)
      if (length(conflict_cols) > 0) {
        # 重命名冲突列(添加前缀super_）
        new_col_names <- ifelse(super_merge_cols %in% conflict_cols,
                                paste0("super_", super_merge_cols),
                                super_merge_cols)
        colnames(super_matched)[match(super_merge_cols, colnames(super_matched))] <- new_col_names
        cleaning_log <- c(cleaning_log, paste0(icon_info, " 已重命名冲突列：",
                                               paste(paste0(conflict_cols, "→super_", conflict_cols), collapse = ",")))
      }

      # 4. 执行合并(此时已无重名列）
      super_merge_cols_renamed <- setdiff(colnames(super_matched), "最终变量名")
      viewer_table <- merge(viewer_table, super_matched[, c("最终变量名", super_merge_cols_renamed)],
                            by = "最终变量名", all.x = TRUE)
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 已合并Super_Single的", length(super_merge_cols_renamed), "列到Viewer$table"))
    }

    # 记录匹配结果
    cleaning_log <- c(cleaning_log,
                      paste0(icon_info, " Super_Single总行数：", total_super_rows),
                      paste0(icon_success, " 匹配成功行数：", matched_count, "(", paste(head(matched_vars, 5), ifelse(matched_count > 5, "...", ""), collapse = ","), "）"),
                      paste0(ifelse(unmatched_count > 0, icon_warning, icon_info),
                             " 匹配失败行数：", unmatched_count, "(", paste(head(unmatched_vars, 5), ifelse(unmatched_count > 5, "...", ""), collapse = ","), "）"))

    if (unmatched_count > 0) {
      warning(paste0(icon_warning, " Super_Single中有", unmatched_count, "行未匹配到Viewer$table的最终变量名"))
    }
  } else if (!is.null(Super_Single) && !"最终变量名" %in% colnames(Super_Single)) {
    warning(paste0(icon_error, " Super_Single缺少'最终变量名'列，无法进行匹配"))
    cleaning_log <- c(cleaning_log, paste0(icon_error, " 匹配失败：Super_Single缺少'最终变量名'列"))
  }

  # 处理add_mark参数
  if (!is.null(add_mark) && ncol(df) > 0) {
    cleaning_log <- c(cleaning_log, paste0(icon_info, " 处理add_mark参数..."))
    if (exists("Tj_add_columns")) {
      current_vars <- final_var_names
      total_vars <- length(current_vars)

      if ("dim" %in% names(add_mark)) {
        existing_dims <- add_mark$dim
        full_dims <- sapply(current_vars, function(var) {
          if (var %in% names(existing_dims)) existing_dims[[var]] else default_mark
        })

        if (length(full_dims) != total_vars) {
          warning(paste0(icon_error, " dim长度异常,强制使用默认值填充"))
          full_dims <- rep(default_mark, total_vars)
          names(full_dims) <- current_vars
        }

        add_mark$dim <- full_dims
        cleaning_log <- c(cleaning_log,
                          paste0(icon_info, " 已补全标记列,共", total_vars, "个变量(",
                                 sum(current_vars %in% names(existing_dims)), "个已有标记,",
                                 sum(!current_vars %in% names(existing_dims)), "个未匹配成功,使用默认值)"))
      }

      viewer_table <- Tj_add_columns(data = viewer_table, add = add_mark)
    } else {
      warning(paste0(icon_warning, " 未找到Tj_add_columns函数,无法处理add_mark"))
      cleaning_log <- c(cleaning_log, paste0(icon_warning, " 警告：未找到Tj_add_columns函数,跳过add_mark处理"))
    }
  }

  # 修正：处理sort_mark对Viewer表格的排序（核心修改部分）
  sorted_vars <- final_var_names  # 初始变量顺序
  if (!is.null(sort_mark) && ncol(viewer_table) > 0) {
    # 检查sort_mark是否为Viewer表格中的有效列名
    valid_sort_cols <- intersect(sort_mark, colnames(viewer_table))
    invalid_sort_cols <- setdiff(sort_mark, colnames(viewer_table))

    if (length(invalid_sort_cols) > 0) {
      warning(paste0(icon_warning, " 以下排序列不存在于Viewer表格中：", paste(invalid_sort_cols, collapse = ",")))
      cleaning_log <- c(cleaning_log, paste0(icon_warning, " 排序列不存在：", paste(invalid_sort_cols, collapse = ",")))
    }

    if (length(valid_sort_cols) > 0) {
      # 按有效列对Viewer表格进行排序
      # 构建排序表达式（支持多列排序）
      sort_expr <- lapply(valid_sort_cols, function(col) viewer_table[[col]])
      # 获取排序后的行索引
      sorted_indices <- do.call(order, sort_expr)
      # 更新Viewer表格顺序
      viewer_table <- viewer_table[sorted_indices, , drop = FALSE]
      # 更新变量顺序为排序后的最终变量名
      sorted_vars <- viewer_table$最终变量名
      cleaning_log <- c(cleaning_log, paste0(icon_success, " 使用sort_mark对Viewer表格排序：", paste(valid_sort_cols, collapse = ",")))
    } else {
      cleaning_log <- c(cleaning_log, paste0(icon_info, " sort_mark无效,使用默认顺序展示Viewer表格"))
    }
  } else if (dynamic_sort && ncol(viewer_table) > 0) {
    # 动态排序逻辑（原逻辑保留）
    valid_other_vars <- if (!is.null(other_vars))
      intersect(other_vars, final_var_names)
    else
      character(0)

    modified_vars <- c(
      if (!is.null(retype)) names(retype),
      if (!is.null(factor_levels)) names(factor_levels),
      if (!is.null(relabel)) names(relabel),
      if (!is.null(rename)) unname(rename)
    )
    modified_vars <- unique(modified_vars)
    modified_vars <- setdiff(modified_vars, valid_other_vars)
    other_vars <- setdiff(final_var_names, c(valid_other_vars, modified_vars))
    sorted_vars <- c(valid_other_vars, modified_vars, other_vars)
    sorted_vars <- intersect(sorted_vars, final_var_names)
    # 按动态排序更新Viewer表格
    viewer_table <- viewer_table[match(sorted_vars, viewer_table$最终变量名), , drop = FALSE]
  } else {
    # 保持原始顺序
    viewer_table <- viewer_table[match(sorted_vars, viewer_table$最终变量名), , drop = FALSE]
  }

  # 准备返回结果
  super_param <- list(
    data = df[, sorted_vars, drop = FALSE],
    label_mapping = label_mapping,
    var_info = list(
      统计信息 = var_stats,
      类型信息 = var_types,
      关键变量 = if (!is.null(other_vars) && ncol(df) > 0) {
        setNames(lapply(other_vars, function(k) {
          if (k %in% final_var_names) list(存在 = TRUE)
          else list(存在 = FALSE)
        }), other_vars)
      } else NULL
    ),
    Viewer = list(table = viewer_table),
    cleaning_log = cleaning_log,
    stats = list(
      样本量 = nrow(df),
      变量数 = ncol(df),
      最终变量名类型 = rename_safe,
      初始变量数 = init_ncol,
      删除变量数 = init_ncol - ncol(df)
    )
  )

  # 输出清洗日志
  cat("\n===== 清洗日志 =====\n")
  cat(paste(super_param$cleaning_log, collapse = "\n"))

  # 显示变量信息表
  if (ncol(df) > 0 && requireNamespace("flextable", quietly = TRUE)) {
    # 移除变量名映射列(原始需求中无此列）
    fftable <- viewer_table  # 直接使用原始必要列表格
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

    # 生成区分度良好的颜色函数
    get_distinct_colors <- function(n) {
      if (n <= 1) return("#E0E0E0")
      hues <- seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    # 仅对color_col_mark指定的列应用颜色
    if (!is.null(color_col_mark)) {
      color_cols <- if (is.character(color_col_mark)) color_col_mark else as.character(color_col_mark)
      valid_cols <- intersect(color_cols, colnames(fftable))
      invalid_cols <- setdiff(color_cols, colnames(fftable))

      if (length(invalid_cols) > 0) {
        warning(paste0(icon_warning, " 以下着色列不存在于表格中：", paste(invalid_cols, collapse = ",")))
      }

      if (length(valid_cols) > 0) {
        for (col in valid_cols) {
          levels <- unique(fftable[[col]])
          levels <- levels[!is.na(levels)]
          n_levels <- length(levels)

          if (n_levels == 0) next

          colors <- get_distinct_colors(n_levels)
          names(colors) <- levels

          cleaning_log <- c(cleaning_log,
                            paste0(icon_info, " 为列'[", col, "]'的", n_levels, "个水平分配颜色"))

          for (level in levels) {
            row_formula_str <- paste0("~ `", gsub("`", "\\`", col), "` == '", gsub("'", "\\'", level), "'")
            col_formula <- as.formula(paste0("~ `", gsub("`", "\\`", col), "`"))

            ft <- ft |> bg(
              i = as.formula(row_formula_str),
              j = col_formula,
              bg = colors[level],
              part = "body"
            )
          }
        }
      }
    }

    print(ft)
  } else if (ncol(df) > 0) {
    message(paste0(icon_info, " 提示：安装flextable包查看格式化表格(install.packages('flextable'))"))
    print(super_param$Viewer$table)
  }

  return(super_param)
}
