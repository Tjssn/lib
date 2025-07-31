


#' 数据变量综合处理与信息管理工具
#'
#' 对输入数据框进行变量处理与管理，包括变量重命名、类型转换、因子水平设置、
#' 变量筛选、标签管理等，并生成详细的变量信息汇总表和清洗日志，为后续数据分析提供标准化数据集。
#' 支持动态可视化，可通过Viewer实时查看处理结果，方便数据质量评估与预处理流程追溯。
#'
#' @section 重要提示:
#' \strong{⚠️} 函数执行过程会生成详细的清洗日志，记录变量处理的每一步操作、转换结果及潜在问题，
#' 建议仔细查看日志信息以确保数据处理符合预期。变量重命名和类型转换操作具有优先级顺序，
#' 请留意参数间的相互影响。
#'
#' @param data 数据框(data.frame)，必填参数。待处理的原始数据集，包含需要进行清洗和转换的所有变量。
#' @param rename_safe 字符向量，可选参数。指定最终变量名的初始格式，决定变量名的基础样式，各可选值及处理规则如下：
#'   "normal_name"：保留中文及合法字符（a-zA-Z0-9_），将所有非法符号（如空格、标点、特殊字符等）转换为下划线；若多个非法符号相邻，仅保留一个下划线；最终变量名通过`make.unique`确保唯一性。
#'   "unicode_name"：将中文及所有非法字符转换为Unicode编码（格式为"u+十六进制值"），仅保留a-zA-Z0-9_等合法字符；最终变量名通过`make.unique`确保唯一性，适合需跨系统兼容的场景。
#'   "original_name"：完全保留原始变量名，不做任何转换；若包含非法符号（如空格、特殊字符等），可能导致后续模型运行时自动重命名或报错。
#'   "label"：使用变量的标签（label）作为初始变量名，若标签不存在则默认使用原始变量名。
#'   默认值为`c("unicode_name", "normal_name", "original_name", "label")`，**建议优先使用"normal_name"**，兼顾可读性与兼容性。
#' @param rename 命名向量，可选参数。用于自定义变量重命名规则，格式为`c("旧最终变量名"="新最终变量名")`。
#'   该操作将覆盖由rename_safe参数设置的初始最终变量名，重命名失败的变量会在日志中提示。
#' @param key_vars 字符向量，可选参数。标记为关键变量的名称（基于`最终变量名`），在动态排序中
#'   关键变量将优先展示在结果的最前面，此外可排除你研究中的关键变量但并不用于本次分析的变量。
#' @param relabel 命名向量，可选参数。用于修改变量的标签信息，格式为`c("最终变量名"="新label")`。
#'   标签主要用于变量信息表中的展示，不影响变量实际名称和数据处理。
#' @param type_conv 命名向量，可选参数。用于指定变量类型转换规则，格式为`c("最终变量名"="目标类型")`。
#'   支持的目标类型包括："numeric"、"integer"、"factor"、"ordered"、"Date"、"character"、"logical"。
#'   若指定的变量不存在，将在日志中提示警告，字符强行转化为因子或数值变量会根据字符的unicode编码对水平进行排序并从1开始计数因子水平。
#' @param factor_levels 列表，可选参数。用于自定义因子变量的水平设置，格式为`list("最终变量名"=list(levels=, labels=, ordered=))`。
#'   其中levels为必须项（指定水平顺序），labels（水平标签）和ordered（是否有序）为可选项，
#'   未指定时默认labels=levels，ordered=FALSE。
#'   **"Tj_to_factor_list2"可将相关数据文件转化为可输入的列表，具体见help(Tj_to_factor_list2)**
#'   **"fast_factor_same"可快速声明多个相同因子赋值的变量，具体见help(fast_factor_same)**
#' @param drop_cols 字符向量，可选参数。指定需要从数据中删除的变量名称（基于最终变量名），
#'   格式为`c("最终变量名1", "最终变量名2")`。删除操作在类型转换和因子设置之后执行。
#' @param add_mark 基于tidy语法，可灵活删改Viewer$table，可选参数。用于在变量信息汇总表中添加自定义标记列，
#'   格式为`list(<your variable1>=list("condition1"),<your variable2>=list("condition2"))`，其中condition支持tidy的mutate语法。
#'
#' @param dynamic_sort 逻辑型，可选参数。是否启用变量动态排序功能。若为TRUE（默认），
#'   变量顺序为"关键变量 > 经过修改的变量 > 其他变量"；若为FALSE，保持原始变量顺序。
#' @param max_level 整数，可选参数。控制变量值展示的最大水平数量，超过该数量时仅显示前N个水平
#'   并提示总数，默认值为5。
#' @param max_level_pos 整数，可选参数。控制变量值展示的最大字符长度，超过该长度将截断并添加"..."，
#'   默认值为30。
#'
#' @details
#' 变量处理优先级：重命名(rename) > 类型转换(type_conv) > 因子设置(factor_levels) > 变量删除(drop_cols)；\cr
#' 变量名生成逻辑：先根据rename_safe生成初始最终变量名，再通过rename参数进行自定义修改；\cr
#' 动态排序规则：关键变量(rename)优先，其次是经过修改的变量（类型转换、因子设置、重命名等），
#' 最后是未修改的普通变量；\cr
#' 日志信息包含：各步骤操作结果、警告信息、错误原因等，便于追踪处理过程和排查问题。
#'
#' @return
#' 列表，包含以下核心组件：
#' \code{data}: 处理后的标准化数据集，变量按指定顺序排列；
#' \code{label_mapping}: 变量名映射表，包含original_name、normal_name、unicode_name、label等信息；
#' \code{var_info}: 变量信息列表，包括统计信息（总观测数、缺失值数量等）和类型信息（基础类型、继承类型等）；
#' \code{Viewer}: 包含格式化的变量信息汇总表，展示变量的各类属性和统计特征；
#' \code{cleaning_log}: 清洗过程的详细日志，记录每一步操作的结果和警告信息；
#' \code{stats}: 数据处理前后的统计对比，包括样本量、变量数、删除变量数等。
#'
#' @note
#' 建议在处理大型数据集前先进行测试，以评估函数性能；\cr
#' 变量名若包含特殊字符，会被自动转换为安全格式，转换规则可通过rename_safe参数控制；\cr
#' 因子水平设置时，请确保levels参数包含所有可能的值，否则会产生NA值；\cr
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
#' # 示例1：初次使用，查看原始数据概况， 若要使用外部Super_single的excel批量处理 请翻到案例最后
#' dat.param1 <- super_param(data = df)
#' # 查看处理后的数据和变量信息表
#' head(dat.param1$data)
#' dat.param1$Viewer$table
#'
#' # 示例2：指定变量名规则（推荐使用"normal"平衡可读性和兼容性）
#' dat.param2 <- super_param(
#'   data = df,
#'   rename_safe = "normal"  # 生成保留中文和合法字符的变量名
#' )
#'
#' # 示例3：自定义重命名与标记关键变量
#' dat.param3 <- super_param(
#'   data = df,
#'   rename_safe = "normal",
#'   key_vars = "ID",  # 标记ID为关键变量，优先展示
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb")  # 自定义重命名规则
#' )
#'
#' # 示例4：为变量添加标签（提升展示可读性）
#' dat.param4 <- super_param(
#'   data = df,
#'   rename_safe = "normal",
#'   key_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   relabel = c(yx = "亚型")  # 为变量yx添加标签"亚型"
#' )
#'
#' # 示例5：设置因子变量的水平与标签
#' dat.param5 <- super_param(
#'   data = df,
#'   rename_safe = "normal",
#'   key_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   relabel = c(yx = "亚型"),
#'   # 自定义因子水平：levels（顺序）、labels（标签）、ordered（是否有序）
#'   factor_levels = list(
#'     y = list(levels = c(1, 2), labels = c("Sruv", "Death")),  # 结局变量
#'     yx = list(levels = 1:5, labels = paste0("亚型", 1:5)),    # 亚型变量
#'     Gender = list(levels = c(1, 2), labels = c("Male", "Female"))  # 性别变量
#'   )
#' )
#' # 查看因子变量转换结果
#' str(dat.param5$data$yx)
#'
#' # 示例6：在变量信息表中添加自定义标记列
#' dat.param6 <- super_param(
#'   data = df,
#'   rename_safe = "normal",
#'   key_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   relabel = c(yx = "亚型"),
#'   factor_levels = list(
#'     y = list(levels = c(1, 2), labels = c("Sruv", "Death")),
#'     yx = list(levels = 1:5, labels = paste0("亚型", 1:5)),
#'     Gender = list(levels = c(1, 2), labels = c("Male", "Female"))
#'   ),
#'   # 添加标记列：dim（变量维度）和cont（是否连续变量）
#'   add_mark = list(
#'     dim = "ifelse(label %in% c('Age','Gender','亚型','Alb','Glo','AGR','Cr'), 1, 2)",
#'     cont = "dplyr::case_when(继承类型 == 'numeric' ~ 1, TRUE ~ 0)"
#'   )
#' )
#' # 查看带标记的变量信息表
#' dat.param6$Viewer$table
#'
#' # 示例7：调整变量值展示的详细程度
#' dat.param7 <- super_param(
#'   data = df,
#'   rename_safe = "normal",
#'   key_vars = "ID",
#'   rename = c(yaxing = "yx", Age = "年龄", var9 = "Alb"),
#'   relabel = c(yx = "亚型"),
#'   factor_levels = list(
#'     y = list(levels = c(1, 2), labels = c("Sruv", "Death")),
#'     yx = list(levels = 1:5, labels = paste0("亚型", 1:5)),
#'     Gender = list(levels = c(1, 2), labels = c("Male", "Female"))
#'   ),
#'   add_mark = list(
#'     dim = "ifelse(label %in% c('Age','Gender','亚型','Alb','Glo','AGR','Cr'), 1, 2)",
#'     cont = "dplyr::case_when(继承类型 == 'numeric' ~ 1, TRUE ~ 0)"
#'   ),
#'   max_level = 10,       # 最多展示10个变量值水平
#'   max_level_pos = 20    # 变量值字符长度超过20时截断
#' )
#'
#' # 示例8：结合外部配置表批量处理（类似SAS super_single逻辑）
#' # 假设已有配置表Super_Single和add_label_rename_format
#' if (exists("Super_Single") && exists("add_label_rename_format")) {
#'   dat.param8 <- super_param(
#'     data = df,
#'     rename_safe = "normal",
#'     # 从配置表提取重命名、标签和类型转换规则
#'     rename = setNames(Super_Single$variable, Super_Single$最终变量名),
#'     relabel = setNames(Super_Single$label, Super_Single$最终变量名),
#'     type_conv = setNames(ifelse(Super_Single$cont == 1, "numeric", "factor"),
#'                         Super_Single$最终变量名),
#'     # 从配置表转换因子水平列表
#'     factor_levels = Tj_factor_list(add_label_rename_format),
#'     # 批量添加标记列
#'     add_mark = list(
#'       dim = Super_Single$dim,
#'       normal = Super_Single$normal
#'     )
#'   )
#' }
#'
#' # 保存处理结果
#' data.lib <- list(raw = df, super = dat.param7)
#' save(data.lib, file = "Super_param_result.rda")
#'
#' @export
super_param <- function(data,
                        type_conv = NULL,
                        factor_levels = NULL,
                        drop_cols = NULL,
                        key_vars = NULL,
                        rename = NULL,
                        rename_safe = c("unicode_name", "normal_name", "original_name", "label"),
                        relabel = NULL,
                        add_mark = NULL,
                        dynamic_sort = T,
                        max_level = 5,
                        max_level_pos = 30) {
  library(flextable)
  if (!inherits(data, "data.frame")) {
    warning("输入数据必须为数据框，返回NULL")
    return(NULL)
  }
  df <- data
  init_nrow <- nrow(df)
  init_ncol <- ncol(df)
  init_raw_vars <- names(df)
  cleaning_log <- character(0)
  safe_exec <- function(expr, msg) {
    tryCatch(
      expr,
      error = function(e) {
        warning(paste("[清洗警告]", msg, "（错误：", e$message, "）"))
        cleaning_log <<- c(cleaning_log, paste("[错误]", msg, "（原因：", e$message, "）"))
        return(NULL)
      }
    )
  }
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

  legal_chars <- "a-zA-Z0-9_\u4e00-\u9fa5"
  normal_name <- sapply(init_raw_vars, function(name) {
    new_name <- gsub(paste0("[^", legal_chars, "]"), "_", name)
    gsub("_+", "_", new_name)
  })
  normal_name <- make.unique(normal_name, sep = "_")

  unicode_name <- sapply(init_raw_vars, function(x)
    get_universal_name(x, init_raw_vars))
  unicode_name <- make.unique(unicode_name, sep = "_")
  label_mapping <- data.frame(
    original_name = init_raw_vars,
    normal_name = normal_name,
    unicode_name = unicode_name,
    label = init_raw_vars,
    stringsAsFactors = FALSE
  )
  rename_safe <- match.arg(rename_safe)
  number = ncol(df)
  initial_final_names <- switch(
    rename_safe,
    "original_name" = label_mapping$original_name,
    "normal_name" = label_mapping$normal_name,
    "unicode_name" = label_mapping$unicode_name,
    "label" = label_mapping$label
  )
  names(df) <- initial_final_names
  cleaning_log <- c(cleaning_log,
                    paste("初始最终变量名（基于", rename_safe, "）：" , number, "个变量"))
  if (!is.null(rename)) {
    cleaning_log <- c(cleaning_log, "基于最终变量名执行自定义重命名...")
    if (is.null(names(rename)) || any(names(rename) == "")) {
      warning("rename参数格式错误：必须为命名向量（如c('旧最终变量名'='新最终变量名')）")
      cleaning_log <- c(cleaning_log, "警告：rename格式错误，跳过")
    } else {
      exist_vars <- names(rename) %in% names(df)
      if (any(!exist_vars)) {
        cleaning_log <- c(cleaning_log, paste("警告：以下最终变量名不存在，无法重命名：", paste(names(rename)[!exist_vars], collapse = ",")))
        rename <- rename[exist_vars]
      }
      if (length(rename) > 0) {
        names(df)[match(names(rename), names(df))] <- rename
        label_mapping$最终变量名 <- names(df)
        cleaning_log <- c(cleaning_log, paste("自定义重命名完成：", paste(
          paste0(names(rename), "→", rename), collapse = ","
        )))
      }
    }
  }
  final_var_names <- names(df)
  if (!is.null(relabel)) {
    cleaning_log <- c(cleaning_log, "基于最终变量名执行label修改...")
    if (is.null(names(relabel)) || any(names(relabel) == "")) {
      warning("relabel参数格式错误：必须为命名向量（如c('最终变量名'='新label')）")
      cleaning_log <- c(cleaning_log, "警告：relabel格式错误，跳过")
    } else {
      var_indices <- match(names(relabel), final_var_names)
      valid_indices <- which(!is.na(var_indices))
      if (length(valid_indices) == 0) {
        cleaning_log <- c(cleaning_log, "警告：未找到匹配的最终变量名，无法修改label")
      } else {
        label_mapping$label[var_indices[valid_indices]] <- relabel[valid_indices]
        cleaning_log <- c(cleaning_log,
                          paste(
                            "成功修改",
                            length(valid_indices),
                            "个变量的label：",
                            paste(paste0(names(relabel)[valid_indices], "→", relabel[valid_indices]), collapse = ",")
                          ))
      }
    }
  }
  if (!is.null(type_conv)) {
    cleaning_log <- c(cleaning_log, "基于最终变量名执行类型转换...")
    success_conv <- character(0)
    for (var in names(type_conv)) {
      if (!var %in% final_var_names) {
        cleaning_log <- c(cleaning_log, paste("警告：最终变量名", var, "不存在，跳过转换"))
        next
      }
      type <- type_conv[var]
      if (!type %in% c("numeric",
                       "integer",
                       "factor",
                       "ordered",
                       "Date",
                       "character",
                       "logical")) {
        cleaning_log <- c(cleaning_log,
                          paste("警告：不支持的类型", type, "，变量", var, "转换失败"))
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
    cleaning_log <- c(cleaning_log, paste("类型转换完成，成功转换", length(success_conv), "个变量"))
  }
  if (!is.null(factor_levels)) {
    cleaning_log <- c(cleaning_log, "基于最终变量名执行因子水平设置...")
    success_factor <- character(0)
    for (var in names(factor_levels)) {
      if (!var %in% final_var_names) {
        cleaning_log <- c(cleaning_log, paste("警告：最终变量名", var, "不存在，跳过因子设置"))
        next
      }
      params <- factor_levels[[var]]
      if (is.null(params$levels)) {
        cleaning_log <- c(cleaning_log, paste("警告：变量", var, "未指定levels，跳过"))
        next
      }
      new_factor <- safe_exec({
        factor(
          x = df[[var]],
          levels = params$levels,
          labels = if (!is.null(params$labels))
            params$labels
          else
            params$levels,
          ordered = if (!is.null(params$ordered))
            params$ordered
          else
            FALSE
        )
      }, paste("变量", var, "因子设置失败"))
      if (!is.null(new_factor)) {
        df[[var]] <- new_factor
        success_factor <- c(success_factor, var)
      }
    }
    cleaning_log <- c(cleaning_log, paste("因子设置完成，成功处理", length(success_factor), "个变量"))
  }
  if (!is.null(drop_cols)) {
    cleaning_log <- c(cleaning_log, "基于最终变量名执行变量删除...")
    drop_exist <- intersect(drop_cols, final_var_names)
    if (length(drop_exist) > 0) {
      drop_indices <- match(drop_exist, final_var_names)
      df <- df[, setdiff(final_var_names, drop_exist), drop = FALSE]
      label_mapping <- label_mapping[-drop_indices, , drop = FALSE]
      final_var_names <- final_var_names[-drop_indices]
      cleaning_log <- c(cleaning_log, paste("删除变量：", paste(drop_exist, collapse = ",")))
    } else {
      cleaning_log <- c(cleaning_log, "无匹配的最终变量名可删除")
    }
  }
  if (dynamic_sort) {
    valid_key_vars <- if (!is.null(key_vars))
      intersect(key_vars, final_var_names)
    else
      character(0)
    modified_vars <- c(
      if (!is.null(type_conv))
        names(type_conv),
      if (!is.null(factor_levels))
        names(factor_levels),
      if (!is.null(relabel))
        names(relabel),
      if (!is.null(rename))
        unname(rename)
    )
    modified_vars <- unique(modified_vars)
    modified_vars <- setdiff(modified_vars, valid_key_vars)
    other_vars <- setdiff(final_var_names, c(valid_key_vars, modified_vars))
    sorted_vars <- c(valid_key_vars, modified_vars, other_vars)
  } else {
    sorted_vars <- final_var_names
  }
  sorted_vars <- intersect(sorted_vars, final_var_names)
  if (ncol(df) == 0) {
    warning("所有变量已被删除，无法生成变量信息表")
    viewer_table <- data.frame()
    var_stats <- list()
    var_types <- list()
  } else {
    # 变量类型分析
    TJ_get_vartype <- function(x) {
      base_type <- class(x)[1]
      inherit_type <- paste(class(x), collapse = "/")
      vals <- unique(x[!is.na(x)])
      val_show <- if (length(vals) > max_level) {
        paste0("共",
               length(vals),
               "个水平（前",
               max_level,
               "个）：",
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
      基础类型 = sapply(type_details, function(x)
        x$基础类型),
      继承类型 = sapply(type_details, function(x)
        x$继承类型),
      变量值展示 = sapply(type_details, function(x)
        x$变量值展示)
    )

    # 变量统计信息
    var_stats <- list(
      总观测数 = sapply(df, function(x)
        length(x)),
      缺失值数量 = sapply(df, function(x)
        sum(is.na(x))),
      有效值数量 = sapply(df, function(x)
        sum(!is.na(x))),
      水平数_唯一值 = sapply(df, function(x) {
        if (inherits(x, "factor"))
          nlevels(x)
        else
          length(unique(x[!is.na(x)]))
      })
    )
    current_original_names <- label_mapping$original_name
    merged_names <- with(label_mapping,
                         paste(original_name, normal_name, unicode_name, sep = "/"))
    names(merged_names) <- current_original_names
    merged_names_sorted <- sapply(sorted_vars, function(var) {
      orig_idx <- match(var, final_var_names)
      if (!is.na(orig_idx)) {
        orig_name <- current_original_names[orig_idx]
        merged_names[orig_name]
      } else {
        NA
      }
    })
    viewer_table <- data.frame(
      最终变量名 = sorted_vars,
      变量名映射 = merged_names_sorted,
      label = label_mapping$label[match(sorted_vars, final_var_names)],
      基础类型 = var_types$基础类型[sorted_vars],
      继承类型 = var_types$继承类型[sorted_vars],
      `总(有效/缺失)` = paste0(
        var_stats$总观测数[sorted_vars],
        " (",
        var_stats$有效值数量[sorted_vars],
        " / ",
        var_stats$缺失值数量[sorted_vars],
        ")"
      ),
      水平数_唯一值 = var_stats$水平数_唯一值[sorted_vars],
      变量值水平 = var_types$变量值展示[sorted_vars],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    viewer_table <- viewer_table[match(sorted_vars, viewer_table$最终变量名), , drop = FALSE]
  }
  if (!is.null(add_mark)) {
    viewer_table <- Tj_add_columns(data = viewer_table , add = add_mark)
  }
  super_param <- list(
    data = df[, sorted_vars, drop = FALSE],
    label_mapping = label_mapping,
    var_info = list(
      统计信息 = var_stats,
      类型信息 = var_types,
      关键变量 = if (!is.null(key_vars) && ncol(df) > 0) {
        setNames(lapply(key_vars, function(k) {
          if (k %in% final_var_names)
            list(存在 = TRUE)
          else
            list(存在 = FALSE)
        }), key_vars)
      } else
        NULL
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
  cat("\n===== 清洗日志 =====\n")
  cat(paste(super_param$cleaning_log, collapse = "\n"))
  if (ncol(df) > 0 &&
      requireNamespace("flextable", quietly = TRUE)) {
    fftable <- super_param$Viewer$table[, -2]
    ft <- flextable(fftable) |>
      theme_box() |>
      set_caption(caption = "Table. Variable information") |>
      hline_top(part = "all",
                border = officer::fp_border(color = "black", width = 1.25)) |>
      hline_bottom(part = "all",
                   border = officer::fp_border(color = "black", width = 1.25)) |>
      bg(
        i = ~ 继承类型 == "character",
        bg = "#E3F2FD",
        part = "body"
      ) |>
      bg(
        i = ~ 继承类型 == "numeric",
        bg = "#D9D9D5",
        part = "body"
      ) |>
      bg(i = ~ 继承类型 == "factor",
         bg = "#FFE8CC",
         part = "body") |>
      autofit() |>
      bold(bold = TRUE, part = c("all")) |>
      italic(italic = TRUE, part = "header") |>
      align(align = "center", part = "all") |>
      valign(valign = "middle", part = "all")
    print(ft)
  } else if (ncol(df) > 0) {
    message("提示：安装flextable包查看格式化表格（install.packages('flextable')）")
    print(super_param$Viewer$table)
  }

  return(super_param)
}
