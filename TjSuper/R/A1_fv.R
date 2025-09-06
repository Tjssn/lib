#' fv
#'
#' 对单个变量进行属性管理与类型转换，支持标签设置、领域分类、排序权重、类型重定义、标识符标记，
#' 以及因子变量的精细化处理（水平设置、标签映射、缺失值替换等）。函数会优先使用用户指定的参数，
#' 若无则继承变量已有属性，否则采用默认值，最终返回带有标准化属性的变量，便于后续数据分析与展示。
#'
#' @section 重要提示:
#' \strong{⚠️} 函数参数优先级规则：用户传入参数 > 变量已有属性 > 函数内部默认值。
#' 类型转换（`reclass`）可能导致数据丢失（如非数值型转为numeric时产生NA），建议检查控制台输出的转换提示；
#' 因子水平设置时，若`factor_labels`未指定则自动与`factor_levels`保持一致，长度不匹配时会提示警告。
#'
#' @param var 变量向量或字符型变量名，必填参数。待处理的变量，若为字符型则需通过`data`参数指定所在数据框。
#' @param data 数据框(data.frame)，可选参数。当`var`为字符型变量名时，用于指定变量所在的数据框；
#'   若`var`为直接传入的向量，则此参数可忽略。
#' @param label 字符型，可选参数。变量的标签信息，用于展示和说明变量含义。默认值为变量名，
#'   若变量已有"label"属性则优先使用。
#' @param domain 字符型，可选参数。变量所属的领域或类别，用于变量归类。默认继承变量已有"domain"属性，
#'   若无则为NA。
#' @param rank 数值型，可选参数。变量的排序权重，用于多变量时的排序依据。默认值为1，
#'   若变量已有"rank"属性则优先使用。
#' @param reclass 字符型，可选参数。指定变量的目标类型，用于类型转换，可选值包括：
#'   "factor"：转换为因子型；
#'   "ordered"：转换为有序因子型；
#'   "numeric"：转换为数值型；
#'   "character"：转换为字符型。
#'   默认继承变量已有"reclass"属性，若无则使用原始类型的第一个类别。
#' @param is_identifier 逻辑型，可选参数。标记变量是否为标识符（如ID变量），用于区分关键标识变量。
#'   默认值为FALSE，若变量已有"is_identifier"属性则优先使用。
#' @param factor_levels 向量，可选参数。指定因子变量的水平顺序，仅当`reclass`为"factor"或"ordered"时有效。
#'   默认继承变量已有"factor_levels"属性，若无则使用变量的唯一值。
#' @param factor_labels 向量，可选参数。指定因子水平的标签，用于替换原始水平的展示名称，
#'   长度需与`factor_levels`一致。默认与`factor_levels`相同，若变量已有"factor_labels"属性则优先使用。
#' @param factor_na 标量，可选参数。用于替换因子变量中的NA值，仅当`reclass`为"factor"或"ordered"时有效。
#'   默认值为NA，若变量已有"factor_na"属性则优先使用。
#' @param as_factor 逻辑型，可选参数。控制是否将变量转换为因子型（当`reclass`为"character"、"factor"或"ordered"时生效）。
#'   若为TRUE则转换为因子，FALSE则保持字符型（仅对"character"有效）。默认值为TRUE，
#'   若变量已有"as_factor"属性则优先使用。
#'
#' @details
#' 变量处理流程：\cr
#' 1. 变量提取：根据`var`类型（向量/变量名）提取目标变量；\cr
#' 2. 属性继承：读取变量已有属性，作为参数默认值（用户输入参数优先）；\cr
#' 3. 类型转换：基于`reclass`参数进行类型转换，处理可能的转换警告（如非数值转numeric产生NA）；\cr
#' 4. 因子处理：若为因子/有序因子，处理NA值替换、水平与标签设置，检测水平合并情况；\cr
#' 5. 属性更新：将所有参数信息写入变量属性，返回带标准化属性的变量。\cr
#'
#' 因子水平合并检测：当`factor_labels`中存在重复值时，函数会自动检测并提示哪些原始水平被合并到同一标签，
#' 便于追踪因子水平的聚合情况。
#'
#' @return
#' 带标准化属性的变量向量，其属性包括：
#' \code{label}: 变量标签；
#' \code{domain}: 所属领域；
#' \code{rank}: 排序权重；
#' \code{reclass}: 目标类型；
#' \code{is_identifier}: 是否为标识符；
#' \code{factor_levels}: 因子水平；
#' \code{factor_labels}: 因子标签；
#' \code{factor_na}: NA替换值；
#' \code{as_factor}: 因子转换标记。
#'
#' @note
#' 若`reclass`指定为无效类型（不在可选值中），函数会使用变量原始类型并提示警告；\cr
#' `factor_na`仅接受长度为1的值，若输入长度超过1则自动使用NA并提示；\cr
#' 转换为numeric时，非数值型值会被转为NA，函数会提示转换过程中产生的NA数量；\cr
#' 有序因子（ordered）的水平展示使用" < "分隔，普通因子使用" , "分隔。
#'
#' @author
#' 开发团队:Tjssn;\cr
#' 联系方式:tongjibbb@163.com;\cr
#' 官方网址:\url{https://study.tjbbb.com};\cr
#' 微信:Tongjissn;\cr
#' 官方平台-公众号:统计碎碎念
#'
#' @examples
#' # 准备工作：创建示例数据
#' set.seed(123)
#' age <- c(25, 30, 35, NA, 40)
#' gender <- c(1, 2, 1, 2, 1)
#'
#' # 示例1：基础使用，处理年龄变量
#' age_processed <- fv(var = age)
#' # 查看处理后的变量及属性
#' age_processed
#' attributes(age_processed)
#'
#' # 示例2：指定标签、重分类为numeric
#' age_processed2 <- fv(
#'   var = age,
#'   label = "年龄(岁)",
#'   reclass = "numeric"
#' )
#' attributes(age_processed2)$label  # 查看标签
#' class(age_processed2)  # 查看类型
#'
#' # 示例3：处理因子变量（性别）
#' gender_processed <- fv(
#'   var = gender,
#'   label = "性别",
#'   reclass = "factor",
#'   factor_levels = c(1, 2),
#'   factor_labels = c("男", "女"),
#'   factor_na = 99  # 假设用99替换NA
#' )
#' gender_processed  # 查看转换后的因子
#' attributes(gender_processed)$factor_labels  # 查看因子标签
#'
#' # 示例4：从数据框中处理变量
#' df <- data.frame(id = 1:5, score = c("85", "90", "NA", "75", "80"))
#' # 处理分数变量：转为numeric，设置标签
#' df$score <- fv(
#'   var = "score",
#'   data = df,
#'   label = "考试分数",
#'   reclass = "numeric"
#' )
#' df$score  # 查看处理后的分数（注意NA值）
#'
#' @export

fv <- function(var,
               data = NULL,
               label = NULL,
               domain = NULL,
               rank = NULL,
               reclass = NULL,
               is_identifier = NULL,
               factor_levels = NULL,
               factor_labels = NULL,
               factor_na = NULL,
               as_factor = NULL
) {

  # 0. 提取变量
  if (is.character(var) && length(var) == 1) {
    if (is.null(data)) {message("Please provide the 'data' parameter"); return(NA)}
    if (!var %in% names(data)) {message("Variable '", var, "' does not exist"); return(NA)}
    x <- data[[var]]
    var_name <- var
  } else {
    x <- var
    var_name <- deparse(substitute(var))
  }

  # 0.1. 核心逻辑：读取变量已有属性，作为参数默认值
  # 优先顺序：用户传入参数 > 变量已有属性 > 函数内部默认值
  get_existing_attr <- function(attr_name, default = NA) {
    if (!is.null(get(attr_name))) {  # 用户传入了参数，用新值
      return(get(attr_name))
    } else if (!is.null(attr(x, attr_name))) {  # 变量有已有属性，用已有值
      return(attr(x, attr_name))
    } else {  # 两者都无，用默认值
      return(default)
    }
  }

  # 读取已有属性，更新参数
  original_type <- class(x)[1]
  label <- get_existing_attr("label", var_name)  # 默认值：变量名
  domain <- get_existing_attr("domain", NA)
  rank <- get_existing_attr("rank", 1)           # 默认值：1
  # 核心修改：class参数改为reclass，读取"reclass"属性（若无可兼容原"class"属性）
  reclass <- get_existing_attr("reclass", if (!is.null(attr(x, "class"))) attr(x, "class")[1] else original_type[1])
  is_identifier <- get_existing_attr("is_identifier", FALSE)  # 默认值：FALSE
  factor_levels <- get_existing_attr("factor_levels", NULL)
  factor_labels <- get_existing_attr("factor_labels", factor_levels)  # 默认：与levels相同
  factor_na <- get_existing_attr("factor_na", NA)
  as_factor <- get_existing_attr("as_factor", TRUE)  # 默认值：TRUE

  # 参数预处理（统一格式）
  label <- unlist(label)
  domain <- unlist(domain)
  reclass <- unlist(reclass)  # 核心修改：class → reclass
  rank <- unlist(rank)
  factor_levels <- unlist(factor_levels)
  factor_labels <- unlist(factor_labels)
  as_factor <- unlist(as_factor)
  factor_na <- unlist(factor_na)
  is_identifier <- unlist(is_identifier)

  # 1. as_factor处理（逻辑不变，基于读取后的as_factor值）
  if (is.na(as_factor) || is.null(as_factor)) {
    attr(x, "as_factor") <- FALSE
  } else if (length(as_factor) > 1) {
    attr(x, "as_factor") <- FALSE
    message(
      "\033[34m", var_name, "\033[0m",
      ": The ",
      "\033[31m", 'as_factor', "\033[0m",
      " must have length 1, but input length is ",
      "\033[31m", length(as_factor), "\033[0m"
    )
  } else if (!is.logical(as_factor)) {
    if (tolower(as.character(as_factor)) %in% c("true", "t")) {
      attr(x, "as_factor") <- TRUE
    } else if (tolower(as.character(as_factor)) %in% c("false", "f")) {
      attr(x, "as_factor") <- FALSE
    } else {
      attr(x, "as_factor") <- FALSE
    }
  } else {
    attr(x, "as_factor") <- as_factor
  }

  # 2. reclass处理（核心修改：原class处理逻辑全部改为reclass）
  final_reclass <- reclass  # 核心修改：final_class → final_reclass
  if (final_reclass %in% c("factor", "ordered", "numeric", "character")) {
    if (final_reclass %in% c("factor", "ordered", "character")) {
      if (isTRUE(as_factor)) {
        if (final_reclass == "character") {
          x <- as.factor(x)
          current_class <- "factor"
        } else if (final_reclass == "factor") {
          x <- as.factor(x)
          current_class <- "factor"
        } else if (final_reclass == "ordered") {
          x <- as.ordered(x)
          current_class <- "ordered"
        }
      } else {
        if (final_reclass == "character") {
          x <- as.character(x)
          current_class <- "character"
          message(
            "\033[34m", var_name, "\033[0m",
            ": 提示：若需将character转为factor，可设置as_factor=TRUE"
          )
        } else {
          # 核心修改：动态调用as.xxx函数，基于final_reclass
          x <- do.call(paste0("as.", final_reclass), list(x))
          current_class <- final_reclass
        }
      }
    } else if (final_reclass == "numeric") {
      if (original_type != "numeric") {
        x_original <- x
        x <- suppressWarnings(as.numeric(x))
        if (any(is.na(x) & !is.na(x_original))) {
          message(
            "\033[34m", var_name, "\033[0m",
            ": 警告：将原始类型[", original_type, "]→[numeric]时产生NA，可能转换错误"
          )
        } else {
          message(
            "\033[34m", var_name, "\033[0m",
            ": 按指定reclass，将原始类型[", original_type, "]→[numeric]"
          )
        }
      } else {
        x <- as.numeric(x)
      }
      current_class <- "numeric"
    }
  } else {
    # 核心修改：无效reclass时的提示信息
    x <- do.call(paste0("as.", original_type[1]), list(x))
    current_class <- original_type[1]
    message(
      "\033[34m", var_name, "\033[0m",
      ": 无效reclass[", reclass, "]，仅允许(factor/numeric/ordered/character)，使用原始类型[", current_class, "]"
    )
  }

  # 核心修改：设置变量的class属性（保持原逻辑，与转换结果一致）
  attr(x, "class") <- current_class
  now_class <- current_class

  # 选择间隔符号（逻辑不变，基于now_class）
  if(now_class == "factor"){
    factor_levels_collapse <- " , "
  }else if(now_class == "ordered"){
    factor_levels_collapse <- " < "
  }

  # 辅助函数：处理超过10个元素的展示（逻辑不变）
  format_log_items <- function(items, collapse) {
    if (length(items) == 0) return("")
    if (length(items) <= 10) {
      paste(items, collapse = collapse)
    } else {
      paste(c(items[1:10], "..."), collapse = collapse)
    }
  }

  # 3. 因子处理（逻辑不变，基于读取后的factor_*参数）
  if(now_class %in% c("ordered", "factor")){
    # 处理NA值（逻辑不变）
    if (length(factor_na) > 1) {
      message(
        "\033[34m", var_name, "\033[0m",
        ": The ",
        "\033[31m", 'factor_na', "\033[0m",
        " length is ",
        "\033[31m", length(factor_na), "\033[0m",
        " (must be 1), default value ",
        "\033[31m", "NA", "\033[0m",
        " is kept."
      )
    } else if (length(factor_na) == 1 && is.na(factor_na)) {
      message(
        "\033[34m", var_name, "\033[0m",
        ": The ",
        "\033[31m", 'factor_na', "\033[0m",
        " is NA, default value ",
        "\033[31m", "NA", "\033[0m",
        " is kept."
      )
    } else if (is.null(factor_na)) {
      message(
        "\033[34m", var_name, "\033[0m",
        ": The ",
        "\033[31m", 'factor_na', "\033[0m",
        " is NULL, default value ",
        "\033[31m", "NA", "\033[0m",
        " is kept."
      )
    } else if (!is.null(factor_na) && !is.na(factor_na) && length(factor_na) == 1) {
      x[is.na(x)] <- factor_na
      message(
        "\033[34m", var_name, "\033[0m",
        ": The ",
        "\033[31m", 'factor_na', "\033[0m",
        " 已将x中的NA替换为：",
        "\033[31m", factor_na, "\033[0m"
      )
    }

    # 处理因子水平和标签（labels默认与levels相同，逻辑不变）
    if (is.null(factor_labels) || all(is.na(factor_labels))) {
      factor_labels <- factor_levels
      # 仅当首次设置或更新时提示（避免重复提示）
      if (is.null(attr(x, "factor_labels")) || !identical(factor_labels, attr(x, "factor_labels"))) {
        message(
          "\033[34m", var_name, "\033[0m",
          ": factor_labels 为 NULL 或 NA，自动设置为与 factor_levels 相同"
        )
      }
    }

    # 处理因子水平（逻辑不变）
    if(all(is.na(factor_levels)) || is.null(factor_levels)){
      unique_vals <- unique(x)
      factor_levels <- unique_vals
      if (is.null(factor_labels) || all(is.na(factor_labels))) {
        factor_labels <- factor_levels
      }
      formatted_levels <- format_log_items(factor_levels, factor_levels_collapse)
      message(
        "\033[34m", var_name, "\033[0m",
        ": The ",
        "\033[31m", 'factor_levels=', "\033[0m",
        "\033[31m", formatted_levels, "\033[0m",
        " is set successfully."
      )
    } else {
      if(length(factor_levels) == length(factor_labels)) {
        merged_groups <- split(factor_levels, factor_labels)
        merged_groups <- merged_groups[sapply(merged_groups, length) > 1]
        unique_labels <- unique(factor_labels)

        # 仅当合并状态变化时提示
        if(length(merged_groups) > 0 && !identical(merged_groups, attr(x, "merged_groups"))) {
          message("\033[34m", var_name, "\033[0m: 检测到因子水平合并:")
          for(label in names(merged_groups)) {
            formatted_merged <- format_log_items(merged_groups[[label]], "、")
            message(
              "\033[34m", var_name, "\033[0m",
              ": 原始水平 ", "\033[32m", formatted_merged, "\033[0m",
              " 已合并，合并后的标签为: ", "\033[33m", label, "\033[0m"
            )
          }
          attr(x, "merged_groups") <- merged_groups  # 记录合并状态，避免重复提示
        } else if (length(merged_groups) == 0 && is.null(attr(x, "merged_groups"))) {
          message("\033[34m", var_name, "\033[0m: 未检测到因子水平合并")
        }

        formatted_labels <- format_log_items(unique_labels, factor_levels_collapse)
        # 仅当标签变化时提示
        if (is.null(attr(x, "factor_labels")) || !identical(unique_labels, attr(x, "factor_labels"))) {
          message(
            "\033[34m", var_name, "\033[0m",
            ": The ",
            "\033[31m", 'factor_labels=', "\033[0m",
            "\033[31m", formatted_labels, "\033[0m",
            " is set successfully."
          )
        }
      } else {
        message(
          "\033[34m", var_name, "\033[0m",
          ": 警告: factor_levels与factor_labels长度不匹配，无法检测合并"
        )
        formatted_levels <- format_log_items(factor_levels, factor_levels_collapse)
        message(
          "\033[34m", var_name, "\033[0m",
          ": The ",
          "\033[31m", 'factor_levels=', "\033[0m",
          "\033[31m", formatted_levels, "\033[0m",
          " is set successfully."
        )
      }
    }
  }

  # 4. 应用因子水平和标签（逻辑不变，基于now_class）
  if (now_class=="ordered"){
    x <- factor(
      x,
      levels=factor_levels,
      labels=factor_labels,
      ordered=TRUE
    )
  }else if(now_class=="factor"){
    x <- factor(
      x,
      levels=factor_levels,
      labels=factor_labels
    )
  }

  # 5. 更新变量属性（保留所有已有属性，仅更新相关字段）
  attr(x,"factor_levels") <- factor_levels
  attr(x,"factor_labels") <- factor_labels
  attr(x,"label") <- label
  attr(x,"domain") <- domain
  attr(x,"rank") <- rank
  attr(x,"is_identifier") <- is_identifier
  attr(x,"as_factor") <- as_factor
  # 核心修改：新增"reclass"属性，记录用户指定的重分类类型
  attr(x,"reclass") <- reclass

  # 6. 其他属性处理（逻辑不变，仅基于读取后的参数）
  # （label、domain、rank、is_identifier 已在属性读取阶段处理，此处无需重复判断）

  return(x)
}
