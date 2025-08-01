update_super_param <- function(
    param_obj,           # 已有的super_param结果对象
    type_conv = NULL,    # 类型转换：c("最终变量名"="类型")
    factor_levels = NULL,# 因子设置：list("最终变量名"=list(levels=, labels=, ordered=))
    drop_cols = NULL,    # 删除变量：c("最终变量名1", "最终变量名2")
    key_vars = NULL,     # 关键变量标记：c("最终变量名1", ...)
    rename = NULL,       # 自定义重命名：c("旧最终变量名"="新最终变量名")
    rename_safe = NULL,  # 最终变量名格式（默认沿用原设置）
    relabel = NULL,      # 修改label：c("最终变量名"="新label")
    add_mark = NULL,     # 添加标记
    dynamic_sort = NULL, # 变量排序逻辑（默认沿用原设置）
    max_level = NULL,    # 变量值展示最大水平数（默认沿用原设置）
    max_level_pos = NULL,# 变量值展示最大长度（默认沿用原设置）
    sort_mark = NULL     # 新增：用于对Viewer表格排序的列名向量
) {
  # 检查输入的param_obj是否有效
  if (!inherits(param_obj, "list") || is.null(param_obj$data) || !inherits(param_obj$data, "data.frame")) {
    stop("param_obj必须是super_param函数返回的有效对象")
  }

  # 初始化追踪日志
  trace_log <- c("开始更新super_param对象")

  # 1. 参数继承处理
  if (is.null(rename_safe)) {
    rename_safe <- param_obj$stats$最终变量名类型
    trace_log <- c(trace_log, paste("继承rename_safe设置:", rename_safe))
  }

  if (is.null(dynamic_sort)) {
    dynamic_sort <- if (!is.null(param_obj$stats$dynamic_sort)) param_obj$stats$dynamic_sort else TRUE
    trace_log <- c(trace_log, paste("继承dynamic_sort设置:", dynamic_sort))
  }

  if (is.null(max_level)) {
    max_level <- if (!is.null(param_obj$stats$max_level)) param_obj$stats$max_level else 5
    trace_log <- c(trace_log, paste("继承max_level设置:", max_level))
  }

  if (is.null(max_level_pos)) {
    max_level_pos <- if (!is.null(param_obj$stats$max_level_pos)) param_obj$stats$max_level_pos else 30
    trace_log <- c(trace_log, paste("继承max_level_pos设置:", max_level_pos))
  }

  # 处理sort_mark继承
  if (is.null(sort_mark) && !is.null(param_obj$stats$sort_mark)) {
    sort_mark <- param_obj$stats$sort_mark
    trace_log <- c(trace_log, paste("继承sort_mark设置:", paste(sort_mark, collapse = ",")))
  }

  # 2. 关键变量处理
  if (is.null(key_vars) && !is.null(param_obj$var_info$关键变量)) {
    key_vars <- names(Filter(function(x) x$存在, param_obj$var_info$关键变量))
    trace_log <- c(trace_log, paste("继承关键变量:", paste(key_vars, collapse = ",")))
  }

  # 3. Label继承（确保与最终变量名对应）
  # 创建原始最终变量名到label的映射
  original_var_to_label <- setNames(
    param_obj$label_mapping$label,
    param_obj$label_mapping$original_name  # 原始变量名作为键
  )
  # 转换为当前最终变量名的映射（通过数据框列名关联）
  current_vars <- names(param_obj$data)
  current_label_mapping <- original_var_to_label[match(current_vars, names(original_var_to_label))]
  names(current_label_mapping) <- current_vars  # 用当前最终变量名作为键

  # 合并新的relabel（新设置覆盖旧设置）
  if (!is.null(relabel)) {
    current_label_mapping[names(relabel)] <- relabel
    trace_log <- c(trace_log, paste("合并新label设置:", paste(names(relabel), collapse = ",")))
  }

  # 4. 提取原始标记列
  original_marks <- NULL
  standard_cols <- c("最终变量名", "变量名映射", "label", "基础类型",
                     "继承类型", "总(有效/缺失)", "水平数_唯一值", "变量值水平")

  if (!is.null(param_obj$Viewer$table) && nrow(param_obj$Viewer$table) > 0) {
    original_viewer_cols <- colnames(param_obj$Viewer$table)
    original_mark_cols <- setdiff(original_viewer_cols, standard_cols)

    if (length(original_mark_cols) > 0) {
      original_marks <- param_obj$Viewer$table[, c("最终变量名", original_mark_cols), drop = FALSE]
      trace_log <- c(trace_log, paste("提取原始标记列:", paste(original_mark_cols, collapse = ",")))
    } else {
      trace_log <- c(trace_log, "未发现原始标记列")
    }
  } else {
    trace_log <- c(trace_log, "原始Viewer表格为空，无标记列可继承")
  }

  # 5. 处理重命名映射（确保标记能跟随变量名变化）
  rename_mapping <- NULL
  if (!is.null(rename)) {
    rename_mapping <- data.frame(
      旧变量名 = names(rename),
      新变量名 = as.character(rename),
      stringsAsFactors = FALSE
    )
    trace_log <- c(trace_log, paste("处理重命名映射:", paste(paste(names(rename), rename, sep="→"), collapse = ",")))
  }

  # 6. 调用super_param生成新对象，传递sort_mark参数
  updated_param <- super_param(
    data = param_obj$data,
    retype = type_conv,  # 注意参数名与super_param保持一致
    factor_levels = factor_levels,
    drop_cols = drop_cols,
    other_vars = key_vars,  # 注意参数名与super_param保持一致
    rename = rename,
    rename_safe = rename_safe,
    relabel = current_label_mapping,  # 使用当前变量名对应的label
    add_mark = add_mark,
    dynamic_sort = dynamic_sort,
    max_level = max_level,
    max_level_pos = max_level_pos,
    sort_mark = sort_mark  # 传递sort_mark参数
  )

  # 7. 合并原始标记列到新Viewer表格
  if (!is.null(original_marks) && !is.null(updated_param$Viewer$table) && nrow(updated_param$Viewer$table) > 0) {
    marks_to_merge <- original_marks

    # 如果有重命名，更新标记中的变量名
    if (!is.null(rename_mapping)) {
      for (i in seq_len(nrow(rename_mapping))) {
        old_var <- rename_mapping$旧变量名[i]
        new_var <- rename_mapping$新变量名[i]
        if (old_var %in% marks_to_merge$最终变量名) {
          marks_to_merge$最终变量名[marks_to_merge$最终变量名 == old_var] <- new_var
          trace_log <- c(trace_log, paste("更新标记列中的变量名:", old_var, "→", new_var))
        }
      }
    }

    # 确保最终变量名在新表格中存在（过滤已删除的变量）
    existing_vars <- updated_param$Viewer$table$最终变量名
    marks_to_merge <- marks_to_merge[marks_to_merge$最终变量名 %in% existing_vars, , drop = FALSE]

    # 执行合并
    updated_param$Viewer$table <- merge(
      updated_param$Viewer$table,
      marks_to_merge,
      by = "最终变量名",
      all.x = TRUE  # 保留新表格中所有变量
    )

    # 确认标记列已合并
    merged_mark_cols <- intersect(original_mark_cols, colnames(updated_param$Viewer$table))
    trace_log <- c(trace_log, paste("成功合并标记列:", paste(merged_mark_cols, collapse = ",")))

    # 恢复列顺序（标准列在前，标记列在后）
    new_standard <- intersect(standard_cols, colnames(updated_param$Viewer$table))
    new_marks <- setdiff(colnames(updated_param$Viewer$table), new_standard)
    updated_param$Viewer$table <- updated_param$Viewer$table[, c(new_standard, new_marks)]

    # 保留排序后的顺序
    if (!is.null(sort_mark) && length(intersect(sort_mark, colnames(updated_param$Viewer$table))) > 0) {
      trace_log <- c(trace_log, "保留sort_mark排序后的表格顺序")
    } else {
      # 未使用sort_mark时恢复变量顺序
      updated_param$Viewer$table <- updated_param$Viewer$table[
        match(names(updated_param$data), updated_param$Viewer$table$最终变量名),
        , drop = FALSE
      ]
    }
  }

  # 8. 合并日志
  updated_param$cleaning_log <- c(
    paste("===== 更新追踪日志 ====="),
    trace_log,
    paste("===== 原始清洗日志 ====="),
    param_obj$cleaning_log,
    paste("===== 新清洗日志 ====="),
    updated_param$cleaning_log
  )

  # 9. 保存参数状态，包括sort_mark
  updated_param$stats$dynamic_sort <- dynamic_sort
  updated_param$stats$max_level <- max_level
  updated_param$stats$max_level_pos <- max_level_pos
  updated_param$stats$sort_mark <- sort_mark  # 保存当前sort_mark设置
  updated_param$stats$inherited_mark_columns <- if(!is.null(original_marks)) original_mark_cols else character(0)

  return(updated_param)
}
