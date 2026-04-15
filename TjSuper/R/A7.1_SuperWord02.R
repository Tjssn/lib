#' SuperWord02
#'
#' 生成结构化 Word 统计分析报告，支持自动插入方法描述、统计方法表、结果解读文本、
#' 主结果表格以及表注内容，并可统一控制标题、正文、表格、表注的字体、字号、
#' 行距、页方向和表格边框样式。
#'
#' `SuperWord02()` 适用于已经完成统计分析、并已整理出标准结果对象的场景。
#' 函数会将结果对象中的表格内容提取出来，按照“报告标题 → 引言 →
#' 统计学方法 → 统计方法表 → 各结果模块说明 → 结果表格 → 表注”的顺序，
#' 输出为 `.docx` Word 报告。
#'
#' @section 重要说明:
#' \itemize{
#'   \item 当前版本优先从 `model_list$Result` 提取结果表；若未提供，则使用 `data_list`。
#'   \item `data_list` 或 `model_list$Result` 中的每个元素应包含 `cross_table`，
#'         函数会使用 `final_data_list[[nm]]$cross_table` 作为最终展示表格。
#'   \item 当 `report_text` 同时包含 `Methods`、`Results`、`TableNotes` 时，
#'         才会启用自动文字报告模式。
#'   \item 当 `show_stat_table = TRUE` 时，`report_text$TableMethod` 不能为空，
#'         否则函数会报错：`table_method is empty.`。
#'   \item 参数 `table_comments` 当前版本已保留在接口中，但函数体中尚未实际使用。
#' }
#'
#' @param model_list 列表，可选。
#'   结果模型对象。当前版本要求其中包含 `Result` 元素，
#'   即函数会使用 `model_list$Result` 作为最终表格数据源。
#'
#' @param data_list 列表，可选。
#'   直接传入的结果表列表。当 `model_list` 不可用时使用。
#'   通常每个元素应为一个结果对象，且至少包含 `cross_table`，
#'   例如：
#'   `list("Table A" = list(cross_table = df1), "Table B" = list(cross_table = df2))`。
#'
#' @param output_path 字符型，必填。
#'   输出 Word 文件路径，建议以 `.docx` 结尾，
#'   例如 `"D:/report/super_table_report.docx"`。
#'
#' @param table_comments 列表，可选。
#'   预留参数。当前版本函数体中尚未实际使用该参数。
#'
#' @param introduction 字符型，默认 `""`。
#'   报告开头的引言或背景描述文字。若为空则不写入引言部分。
#'
#' @param report_text 列表，可选。
#'   自动文字报告对象。若包含 `Methods`、`Results`、`TableNotes`
#'   三个命名元素，则启用自动报告模式。
#'
#'   推荐结构如下：
#'   \describe{
#'     \item{Methods}{统计方法部分。可包含：
#'       `heading1`、`chinese`、`english`}
#'     \item{TableMethod}{统计方法表列表。仅在 `show_stat_table = TRUE` 时使用；
#'       每个元素应为 data.frame}
#'     \item{Results}{各结果模块的说明文字列表。
#'       每个表名下可包含 `chinese` 和 `english`}
#'     \item{TableNotes}{各结果表的表注列表。
#'       每个表名下可包含 `chinese` 和 `english`}
#'   }
#'
#' @param show_stat_table 逻辑值，默认 `TRUE`。
#'   是否在 Word 报告中插入“统计方法表”。
#'   若为 `TRUE`，则要求 `report_text$TableMethod` 存在且非空。
#'
#' @param hline.bottom.table 数值，默认 `1.5`。
#'   表格底部边框线宽。
#'
#' @param hline.top.table 数值，默认 `1.5`。
#'   表格顶部边框线宽。
#'
#' @param hline.middle.table 数值，默认 `0.75`。
#'   表头下方中间横线线宽。
#'
#' @param text.title.size 报告总标题字号，默认 `14`。
#' @param text.heading1.size 一级标题字号，默认 `13`。
#' @param text.heading2.size 二级标题字号，默认 `12`。
#' @param text.intro.size 引言正文字号，默认 `12`。
#' @param text.textcom.size 结果说明文字字号，默认 `12`。
#' @param text.table.caption.size 表题字号，默认 `12`。
#' @param text.table.content.size 表格内容字号，默认 `11`。
#' @param text.table.note.size 表注字号，默认 `10`。
#'
#' @param line.title.spacing 报告总标题行距，默认 `1`。
#' @param line.heading1.spacing 一级标题行距，默认 `1.2`。
#' @param line.heading2.spacing 二级标题行距，默认 `1.1`。
#' @param line.intro.spacing 引言正文行距，默认 `1.3`。
#' @param line.textcom.spacing 结果说明文字行距，默认 `1.3`。
#' @param line.caption.spacing 表题行距，默认 `1`。
#' @param line.table.note.spacing 表注行距，默认 `1.2`。
#'
#' @param text.title.family 报告总标题字体，默认 `"Times New Roman"`。
#' @param text.heading1.family 一级标题字体，默认 `"Times New Roman"`。
#' @param text.heading2.family 二级标题字体，默认 `"Times New Roman"`。
#' @param text.intro.family 引言字体，默认 `"Times New Roman"`。
#' @param text.textcom.family 结果说明文字字体，默认 `"Times New Roman"`。
#' @param text.table.caption.family 表题字体，默认 `"Times New Roman"`。
#' @param text.table.content.family 表格内容字体，默认 `"Times New Roman"`。
#' @param text.table.note.family 表注字体，默认 `"Times New Roman"`。
#'
#' @param pad.table.cell 数值，默认 `1`。
#'   表格单元格内边距。
#'
#' @param page_orientation 页面方向，默认 `c("portrait", "landscape")`。
#'   可选 `"portrait"` 或 `"landscape"`，内部通过 `match.arg()` 匹配。
#'
#' @details
#' 函数主要流程如下：
#' \enumerate{
#'   \item 检查 `flextable`、`officer`、`dplyr` 是否已安装；
#'   \item 创建 Word 文档对象；
#'   \item 根据 `page_orientation` 设置纵向或横向页面；
#'   \item 解析数据源：优先使用 `model_list$Result`，否则使用 `data_list`；
#'   \item 写入总标题 `"Statistical Report / 统计分析报告"`；
#'   \item 若 `introduction` 非空，则写入引言；
#'   \item 若 `report_text` 结构完整，则写入统计方法部分；
#'   \item 若 `show_stat_table = TRUE`，写入 `report_text$TableMethod` 中的统计方法表；
#'   \item 遍历每个结果模块，依次写入二级标题、结果说明、结果表格及表注；
#'   \item 保存 Word 文件到 `output_path`。
#' }
#'
#' 表格格式规则：
#' \itemize{
#'   \item 默认移除所有边框后，添加顶部线、表头下横线、底部线；
#'   \item 表头加粗并居中；
#'   \item 若表中存在 `row_id` 列，则按 `row_id == 1` 识别主变量行；
#'   \item 若存在 `Variable` 列，则优先将其作为左侧标签列；
#'         否则若存在 `ROW` 列，则使用 `ROW`；
#'         若都不存在，则使用第一列作为标签列；
#'   \item 非主变量行的标签列会自动增加左缩进。
#' }
#'
#' @return
#' 函数主要作用是将 Word 文档写入 `output_path`。
#' 当前版本不显式返回分析结果对象，通常返回 `NULL`（不可见）。
#'
#' @seealso
#' \code{\link[flextable]{flextable}},
#' \code{\link[officer]{read_docx}},
#' \code{\link[officer]{body_add_flextable}}
#'
#' @examples
#' \dontrun{
#' # 方式1：直接使用结果表列表
#' SuperWord02(
#'   data_list = Result1$result$table$model.res,
#'   output_path = "D:/superword02_report.docx",
#'   introduction = "This report summarizes the statistical analysis results."
#' )
#'
#' # 方式2：提供自动文字报告
#' SuperWord02(
#'   data_list = Result1$result$table$model.res,
#'   output_path = "D:/superword02_auto_report.docx",
#'   introduction = "This report summarizes the statistical analysis results.",
#'   report_text = Result1$result$output.infor$word.text.res,
#'   show_stat_table = TRUE
#' )
#'
#' # 方式3：使用 model_list$Result
#' SuperWord02(
#'   model_list = list(Result = Result1$result$table$model.res),
#'   output_path = "D:/superword02_model_report.docx",
#'   show_stat_table = FALSE,
#'   page_orientation = "landscape"
#' )
#' }
#'
#' @export
#'
SuperWord02 <- function(
    model_list = NULL,
    data_list = NULL,
    output_path,
    table_comments = NULL,
    introduction = "",
    report_text = NULL,
    show_stat_table = TRUE,
    hline.bottom.table = 1.5,
    hline.top.table = 1.5,
    hline.middle.table = 0.75,
    text.title.size = 14,
    text.heading1.size = 13,
    text.heading2.size = 12,
    text.intro.size = 12,
    text.textcom.size = 12,
    text.table.caption.size = 12,
    text.table.content.size = 11,
    text.table.note.size = 10,
    line.title.spacing = 1,
    line.heading1.spacing = 1.2,
    line.heading2.spacing = 1.1,
    line.intro.spacing = 1.3,
    line.textcom.spacing = 1.3,
    line.caption.spacing = 1,
    line.table.note.spacing = 1.2,
    text.title.family = "Times New Roman",
    text.heading1.family = "Times New Roman",
    text.heading2.family = "Times New Roman",
    text.intro.family = "Times New Roman",
    text.textcom.family = "Times New Roman",
    text.table.caption.family = "Times New Roman",
    text.table.content.family = "Times New Roman",
    text.table.note.family = "Times New Roman",
    pad.table.cell = 1,
    page_orientation = c("portrait", "landscape")
) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required.", call. = FALSE)
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  library(flextable)
  library(officer)
  library(dplyr)

  page_orientation <- match.arg(page_orientation)
  doc <- read_docx()

  if (page_orientation == "landscape") {
    doc <- body_set_default_section(
      doc,
      prop_section(
        page_size = page_size(orient = "landscape"),
        type = "continuous"
      )
    )
  }

  use_auto_report <- !is.null(report_text) &&
    all(c("Methods", "Results", "TableNotes") %in% names(report_text))

  if (!is.null(model_list) && !is.null(model_list$Result)) {
    final_data_list <- model_list$Result
  } else if (!is.null(data_list)) {
    final_data_list <- data_list
  } else {
    stop("Please provide valid data_list or model_list.", call. = FALSE)
  }

  normalize_text_block <- function(x) {
    if (is.null(x)) return(character(0))
    x <- as.character(x)
    x <- x[!is.na(x)]
    x <- trimws(x)
    x <- x[nzchar(x)]
    x
  }

  add_spacer <- function(doc) {
    body_add_par(doc, "", style = "Normal")
  }

  add_heading <- function(doc, text, level = 1, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)

    if (level == 1) {
      doc <- body_add_fpar(
        doc,
        fpar(
          ftext(
            as.character(text),
            fp_text(
              font.family = text.heading1.family,
              font.size = text.heading1.size,
              bold = TRUE
            )
          ),
          fp_p = fp_par(
            text.align = "left",
            line_spacing = line.heading1.spacing
          )
        )
      )
    } else {
      doc <- body_add_fpar(
        doc,
        fpar(
          ftext(
            as.character(text),
            fp_text(
              font.family = text.heading2.family,
              font.size = text.heading2.size,
              bold = TRUE
            )
          ),
          fp_p = fp_par(
            text.align = "left",
            line_spacing = line.heading2.spacing
          )
        )
      )
    }

    if (isTRUE(spacer_after)) {
      doc <- add_spacer(doc)
    }
    doc
  }

  add_paragraph <- function(doc, text, family, size, spacing = 1.3, bold = FALSE, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)

    doc <- body_add_fpar(
      doc,
      fpar(
        ftext(
          as.character(text),
          fp_text(
            font.family = family,
            font.size = size,
            bold = bold
          )
        ),
        fp_p = fp_par(
          text.align = "justify",
          line_spacing = spacing
        )
      )
    )

    if (isTRUE(spacer_after)) {
      doc <- add_spacer(doc)
    }
    doc
  }

  add_paragraph_block <- function(doc, text_vec, family, size, spacing = 1.3, bold = FALSE, spacer_after_block = TRUE) {
    txts <- normalize_text_block(text_vec)
    if (length(txts) == 0) return(doc)

    for (i in seq_along(txts)) {
      doc <- add_paragraph(
        doc,
        txts[i],
        family = family,
        size = size,
        spacing = spacing,
        bold = bold,
        spacer_after = FALSE
      )
    }

    if (isTRUE(spacer_after_block)) {
      doc <- add_spacer(doc)
    }
    doc
  }

  add_table_caption <- function(doc, text, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)

    doc <- body_add_fpar(
      doc,
      fpar(
        ftext(
          as.character(text),
          fp_text(
            font.family = text.table.caption.family,
            font.size = text.table.caption.size,
            bold = TRUE
          )
        ),
        fp_p = fp_par(
          text.align = "center",
          line_spacing = line.caption.spacing
        )
      )
    )

    if (isTRUE(spacer_after)) {
      doc <- add_spacer(doc)
    }
    doc
  }

  format_ft_by_rowid <- function(ft, df) {
    has_row_id <- "row_id" %in% colnames(df)

    row_label_col <- if ("Variable" %in% colnames(df)) {
      "Variable"
    } else if ("ROW" %in% colnames(df)) {
      "ROW"
    } else {
      colnames(df)[1]
    }

    if (has_row_id) {
      other_cols <- setdiff(colnames(df), c("row_id", row_label_col))

      ft <- ft %>%
        bold(i = ~ row_id == 1, j = row_label_col, bold = TRUE, part = "body") %>%
        align(i = ~ row_id == 1, j = row_label_col, align = "left", part = "body")

      if (length(other_cols) > 0) {
        ft <- ft %>%
          align(i = ~ row_id == 1, j = other_cols, align = "center", part = "body")
      }

      ft <- ft %>%
        align(i = ~ row_id != 1, j = row_label_col, align = "left", part = "body") %>%
        padding(i = ~ row_id != 1, j = row_label_col, padding.left = 24, part = "body")

      if (length(other_cols) > 0) {
        ft <- ft %>%
          align(i = ~ row_id != 1, j = other_cols, align = "center", part = "body")
      }

      ft <- ft %>% delete_columns(j = "row_id")
    } else {
      other_cols <- setdiff(colnames(df), row_label_col)

      ft <- ft %>%
        align(j = row_label_col, align = "left", part = "body")

      if (length(other_cols) > 0) {
        ft <- ft %>%
          align(j = other_cols, align = "center", part = "body")
      }
    }

    ft
  }

  build_ft <- function(df) {
    flextable(df) %>%
      border_remove() %>%
      hline_top(part = "all", border = fp_border(color = "black", width = hline.top.table)) %>%
      hline_bottom(part = "header", border = fp_border(color = "black", width = hline.middle.table)) %>%
      hline_bottom(part = "all", border = fp_border(color = "black", width = hline.bottom.table)) %>%
      font(part = "all", fontname = text.table.content.family) %>%
      fontsize(part = "all", size = text.table.content.size) %>%
      bold(part = "header", bold = TRUE) %>%
      align(part = "header", align = "center") %>%
      padding(part = "all", padding = pad.table.cell) %>%
      set_table_properties(layout = "autofit", width = 1)
  }

  doc <- body_add_fpar(
    doc,
    fpar(
      ftext(
        "Statistical Report / 统计分析报告",
        fp_text(
          font.family = text.title.family,
          font.size = text.title.size,
          bold = TRUE
        )
      ),
      fp_p = fp_par(
        text.align = "center",
        line_spacing = line.title.spacing
      )
    )
  )
  doc <- add_spacer(doc)

  if (nzchar(trimws(as.character(introduction)))) {
    doc <- add_paragraph(
      doc,
      introduction,
      family = text.intro.family,
      size = text.intro.size,
      spacing = line.intro.spacing,
      bold = FALSE,
      spacer_after = TRUE
    )
  }

  if (use_auto_report) {
    meth <- report_text$Methods
    doc <- add_heading(doc, meth$heading1 %||% "Statistical Methods / 统计学方法", level = 1, spacer_after = TRUE)

    doc <- add_paragraph_block(
      doc,
      meth$chinese,
      family = text.intro.family,
      size = text.intro.size,
      spacing = line.intro.spacing,
      bold = FALSE,
      spacer_after_block = TRUE
    )

    doc <- add_paragraph_block(
      doc,
      meth$english,
      family = text.intro.family,
      size = text.intro.size,
      spacing = line.intro.spacing,
      bold = FALSE,
      spacer_after_block = TRUE
    )
  }

  table_method <- report_text$TableMethod
  if (show_stat_table) {
    if (is.null(table_method) || length(table_method) == 0) {
      stop("table_method is empty.", call. = FALSE)
    }

    method_names <- names(table_method)
    if (is.null(method_names) || any(method_names == "")) {
      method_names <- paste0("Table ", seq_along(table_method))
    }

    for (i in seq_along(table_method)) {
      stat_df <- table_method[[i]]
      stat_title <- method_names[i]

      doc <- add_table_caption(doc, stat_title, spacer_after = TRUE)
      ft_tmp <- build_ft(stat_df) %>% align(part = "body", align = "center")
      doc <- body_add_flextable(doc, ft_tmp)
      doc <- add_spacer(doc)
    }

    doc <- body_add_break(doc)
  }

  if (use_auto_report && !is.null(report_text$Results)) {
    doc <- add_heading(doc, "Results Summary / 结果概述", level = 1, spacer_after = TRUE)
  }

  tabid <- 1
  table_names <- names(final_data_list)

  for (nm in table_names) {
    if (tabid > 1) {
      doc <- body_add_break(doc)
    }

    doc <- add_heading(doc, paste0("Results for ", nm, " / ", nm, " 的结果"), level = 2, spacer_after = TRUE)

    if (use_auto_report && !is.null(report_text$Results[[nm]])) {
      sec <- report_text$Results[[nm]]

      doc <- add_paragraph_block(
        doc,
        sec$chinese,
        family = text.textcom.family,
        size = text.textcom.size,
        spacing = line.textcom.spacing,
        bold = FALSE,
        spacer_after_block = TRUE
      )

      doc <- add_paragraph_block(
        doc,
        sec$english,
        family = text.textcom.family,
        size = text.textcom.size,
        spacing = line.textcom.spacing,
        bold = FALSE,
        spacer_after_block = TRUE
      )
    }

    df <- as.data.frame(final_data_list[[nm]]$cross_table, stringsAsFactors = FALSE)

    doc <- add_table_caption(doc, paste0("Table ", tabid, ". ", nm), spacer_after = TRUE)

    ft <- build_ft(df)
    ft <- format_ft_by_rowid(ft, df)

    doc <- body_add_flextable(doc, ft)

    # 这里不再插入空行，确保表格和表注紧贴
    if (use_auto_report && !is.null(report_text$TableNotes[[nm]])) {
      tn <- report_text$TableNotes[[nm]]

      doc <- add_paragraph_block(
        doc,
        tn$chinese,
        family = text.table.note.family,
        size = text.table.note.size,
        spacing = line.table.note.spacing,
        bold = FALSE,
        spacer_after_block = FALSE
      )

      doc <- add_paragraph_block(
        doc,
        tn$english,
        family = text.table.note.family,
        size = text.table.note.size,
        spacing = line.table.note.spacing,
        bold = FALSE,
        spacer_after_block = FALSE
      )
    }

    tabid <- tabid + 1
  }

  print(doc, target = output_path)
  message("Word report generated successfully: ", output_path)
}
