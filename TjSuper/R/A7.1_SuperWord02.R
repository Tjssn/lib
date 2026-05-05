#' 生成中英文双语 Word 统计报告
#'
#' `SuperWord.Super.Table()` 用于根据统计分析结果表自动生成 Microsoft Word
#' 报告。该函数支持中文和英文双语报告布局、自动统计方法文字、
#' 结果概述、表格标题、表格注释、字体设置、字号设置、行距设置、
#' 表格边框设置以及页面方向设置。
#'
#' 该函数主要用于将一个或多个交叉表结果整理为规范的 Word 报告。
#' 输入结果可以来自 `model_list$Result`，也可以直接通过 `data_list`
#' 提供。
#'
#' @param model_list 可选列表。通常为模型或统计分析函数的输出对象。
#'   如果该对象中包含 `Result` 元素，则函数会使用 `model_list$Result`
#'   作为报告表格来源。
#' @param data_list 可选列表。直接提供的统计结果列表。列表中的每个元素
#'   应包含一个 `cross_table` 数据框，用于生成 Word 报告中的表格。
#'   当未提供有效的 `model_list$Result` 时，函数会使用 `data_list`。
#' @param output_path 字符串。生成的 Word 文件保存路径，通常以 `.docx`
#'   结尾。
#' @param table_comments 可选列表。每张表格下方额外添加的注释文字。
#'   如果该列表有名称，名称应与 `data_list` 或 `model_list$Result`
#'   中的表格名称一致。
#' @param introduction 字符串。报告开头的引言文字。
#' @param report_text 可选列表。自动生成的报告文字。若提供，通常应包含
#'   `Methods`、`Results` 和 `TableNotes` 三个元素，也可以包含
#'   `TableMethod` 元素。
#' @param show_stat_table 逻辑值。当 `report_text$TableMethod` 存在时，
#'   是否在报告中显示统计方法表。默认为 `TRUE`。
#'
#' @param hline.bottom.table 数值。表格底部横线的宽度。
#' @param hline.top.table 数值。表格顶部横线的宽度。
#' @param hline.middle.table 数值。表头下方横线的宽度。
#'
#' @param text.title.size 数值。报告标题的字号。
#' @param text.heading1.size 数值。一级标题的字号。
#' @param text.heading2.size 数值。二级标题的字号。
#' @param text.intro.size 数值。引言和统计方法正文的字号。
#' @param text.textcom.size 数值。结果概述正文的字号。
#' @param text.table.caption.size 数值。表格标题的字号。
#' @param text.table.content.size 数值。表格主体内容的字号。
#' @param text.table.superscript.size 数值。表格中上标标记的字号，
#'   例如两两比较标记中的字母或星号。
#' @param text.table.note.size 数值。表格注释文字的字号。
#'
#' @param line.title.spacing 数值。报告标题的行距。
#' @param line.heading1.spacing 数值。一级标题的行距。
#' @param line.heading2.spacing 数值。二级标题的行距。
#' @param line.intro.spacing 数值。引言和统计方法正文的行距。
#' @param line.textcom.spacing 数值。结果概述正文的行距。
#' @param line.caption.spacing 数值。表格标题的行距。
#' @param line.table.note.spacing 数值。表格注释文字的行距。
#'
#' @param text.title.family 字符串。报告标题字体。默认为
#'   `"Times New Roman"`。
#' @param text.heading1.family 字符串。一级标题字体。
#' @param text.heading2.family 字符串。二级标题字体。
#' @param text.intro.family 字符串。引言和统计方法正文字体。
#' @param text.textcom.family 字符串。结果概述正文字体。
#' @param text.table.caption.family 字符串。表格标题字体。
#' @param text.table.content.family 字符串。表格主体内容字体。
#' @param text.table.note.family 字符串。表格注释字体。
#'
#' @param pad.table.cell 数值。Word 表格单元格内边距。
#' @param page_orientation 字符串。页面方向，可选 `"portrait"` 或
#'   `"landscape"`。`"portrait"` 表示纵向页面，`"landscape"` 表示横向页面。
#' @param bilingual_layout 字符串。双语报告布局方式，可选
#'   `"cn_then_en"` 或 `"legacy"`。`"cn_then_en"` 表示先生成中文部分，
#'   如果存在英文内容，再生成英文部分；`"legacy"` 表示兼容旧版本的
#'   紧凑布局。
#' @param repeat_tables_in_english 逻辑值。是否在英文部分重复显示详细表格。
#'   默认为 `FALSE`。当设为 `FALSE` 时，英文部分通常只显示文字说明，
#'   并提示详细表格见中文部分。
#'
#' @details
#' `data_list` 或 `model_list$Result` 中的每个元素应至少包含一个
#' `cross_table` 数据框。例如：
#'
#' \preformatted{
#' data_list <- list(
#'   Baseline = list(
#'     cross_table = data.frame(...)
#'   )
#' )
#' }
#'
#' 如果表格中存在 `row_id` 列，函数会根据 `row_id` 对表格行进行格式化。
#' 一般情况下，`row_id == 1` 的行会被视为主变量行，`row_id != 1`
#' 的行会被视为分类水平行，并在 Word 表格中进行缩进显示。
#'
#' 如果表格中存在 p 值列，例如 `pvalue`、`p_value`、`p.value`、`p`
#' 或 `p值`，函数会将该列的表头统一显示为斜体 `P`。
#'
#' 对于表格单元格中以字母或星号结尾的统计值，函数会尝试将末尾的
#' 标记设置为上标格式。这通常用于显示两两比较标记。
#'
#' 该函数依赖 `officer` 和 `flextable` 包生成 Word 文档和格式化表格。
#'
#' @return 不可见地返回 `output_path`。函数会在指定路径生成一个
#'   Word `.docx` 文件。
#'
#' @seealso
#' [officer::read_docx()], [flextable::flextable()]
#'
#' @examples
#' \dontrun{
#' example_table <- data.frame(
#'   Variable = c("Age", "Sex", "Male", "Female"),
#'   Group1 = c("30.2 (5.1)", "", "50 (50.0%)", "50 (50.0%)"),
#'   Group2 = c("31.4 (4.8)", "", "45 (45.0%)", "55 (55.0%)"),
#'   P = c("0.120", "", "0.480", ""),
#'   row_id = c(1, 1, 2, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' data_list <- list(
#'   Baseline = list(cross_table = example_table)
#' )
#'
#' output_file <- tempfile(fileext = ".docx")
#'
#' SuperWord.Super.Table(
#'   data_list = data_list,
#'   output_path = output_file,
#'   introduction = "这是一个示例统计分析报告。"
#' )
#' }
#'
#' @export
SuperWord.Super.Table <- function(
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
    text.table.superscript.size = 14,
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
    page_orientation = c("portrait", "landscape"),
    bilingual_layout = c("cn_then_en", "legacy"),
    repeat_tables_in_english = FALSE
) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required.", call. = FALSE)
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required.", call. = FALSE)
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a

  page_orientation <- match.arg(page_orientation)
  bilingual_layout <- match.arg(bilingual_layout)

  doc <- officer::read_docx()

  if (page_orientation == "landscape") {
    doc <- officer::body_set_default_section(
      doc,
      officer::prop_section(
        page_size = officer::page_size(orient = "landscape"),
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
    x[nzchar(x)]
  }

  add_spacer <- function(doc) {
    officer::body_add_par(doc, "", style = "Normal")
  }

  add_title <- function(doc, text) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          as.character(text),
          officer::fp_text(
            font.family = text.title.family,
            font.size = text.title.size,
            bold = TRUE
          )
        ),
        fp_p = officer::fp_par(
          text.align = "center",
          line_spacing = line.title.spacing
        )
      )
    )
    add_spacer(doc)
  }

  add_heading <- function(doc, text, level = 1, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)
    fam <- if (level == 1) text.heading1.family else text.heading2.family
    sz <- if (level == 1) text.heading1.size else text.heading2.size
    sp <- if (level == 1) line.heading1.spacing else line.heading2.spacing

    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          as.character(text),
          officer::fp_text(font.family = fam, font.size = sz, bold = TRUE)
        ),
        fp_p = officer::fp_par(text.align = "left", line_spacing = sp)
      )
    )
    if (isTRUE(spacer_after)) doc <- add_spacer(doc)
    doc
  }

  add_paragraph <- function(doc, text, family, size, spacing = 1.3, bold = FALSE, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          as.character(text),
          officer::fp_text(font.family = family, font.size = size, bold = bold)
        ),
        fp_p = officer::fp_par(text.align = "justify", line_spacing = spacing)
      )
    )
    if (isTRUE(spacer_after)) doc <- add_spacer(doc)
    doc
  }

  collapse_to_paragraph <- function(text_vec, lang = c("auto", "cn", "en")) {
    lang <- match.arg(lang)
    txts <- normalize_text_block(text_vec)
    if (length(txts) == 0) return(character(0))
    if (length(txts) == 1) return(txts)

    if (identical(lang, "en")) {
      return(paste(txts, collapse = " "))
    }
    if (identical(lang, "cn")) {
      return(paste0(txts, collapse = ""))
    }

    has_cn <- any(grepl("[\u4e00-\u9fff]", txts, perl = TRUE))
    if (isTRUE(has_cn)) paste0(txts, collapse = "") else paste(txts, collapse = " ")
  }

  add_paragraph_block <- function(doc, text_vec, family, size, spacing = 1.3, bold = FALSE,
                                  spacer_after_block = TRUE, lang = c("auto", "cn", "en")) {
    lang <- match.arg(lang)
    paragraph <- collapse_to_paragraph(text_vec, lang = lang)
    if (length(paragraph) == 0 || !nzchar(trimws(paragraph))) return(doc)

    doc <- add_paragraph(
      doc,
      paragraph,
      family = family,
      size = size,
      spacing = spacing,
      bold = bold,
      spacer_after = FALSE
    )
    if (isTRUE(spacer_after_block)) doc <- add_spacer(doc)
    doc
  }

  add_table_caption <- function(doc, text, spacer_after = TRUE) {
    if (is.null(text) || !nzchar(trimws(as.character(text)))) return(doc)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          as.character(text),
          officer::fp_text(
            font.family = text.table.caption.family,
            font.size = text.table.caption.size,
            bold = TRUE
          )
        ),
        fp_p = officer::fp_par(text.align = "center", line_spacing = line.caption.spacing)
      )
    )
    if (isTRUE(spacer_after)) doc <- add_spacer(doc)
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
      ft <- flextable::bold(ft, i = ~ row_id == 1, j = row_label_col, bold = TRUE, part = "body")
      ft <- flextable::align(ft, i = ~ row_id == 1, j = row_label_col, align = "left", part = "body")
      if (length(other_cols) > 0) ft <- flextable::align(ft, i = ~ row_id == 1, j = other_cols, align = "center", part = "body")
      ft <- flextable::align(ft, i = ~ row_id != 1, j = row_label_col, align = "left", part = "body")
      ft <- flextable::padding(ft, i = ~ row_id != 1, j = row_label_col, padding.left = 24, part = "body")
      if (length(other_cols) > 0) ft <- flextable::align(ft, i = ~ row_id != 1, j = other_cols, align = "center", part = "body")
      ft <- flextable::delete_columns(ft, j = "row_id")
    } else {
      other_cols <- setdiff(colnames(df), row_label_col)
      ft <- flextable::align(ft, j = row_label_col, align = "left", part = "body")
      if (length(other_cols) > 0) ft <- flextable::align(ft, j = other_cols, align = "center", part = "body")
    }
    ft
  }

  apply_pairwise_mark_superscript <- function(ft, df) {
    special_cols <- c("row_id", "ROW", "Variable", "statistic", "pvalue", "Stat", "P", "SMD", "95%CI")
    target_cols <- setdiff(colnames(df), special_cols)
    target_cols <- target_cols[target_cols %in% ft$col_keys]
    if (length(target_cols) == 0) return(ft)

    normal_prop <- officer::fp_text(font.family = text.table.content.family, font.size = text.table.content.size)
    super_prop <- officer::fp_text(
      font.family = text.table.content.family,
      font.size = if (is.null(text.table.superscript.size)) text.table.content.size else text.table.superscript.size,
      vertical.align = "superscript"
    )

    for (j_col in target_cols) {
      for (i_row in seq_len(nrow(df))) {
        val <- as.character(df[[j_col]][i_row])
        if (is.na(val) || !nzchar(val)) next
        m <- regexec("^(.+[0-9\\)\\]%])([A-Za-z*]+)$", val, perl = TRUE)
        hit <- regmatches(val, m)[[1]]
        if (length(hit) == 3) {
          ft <- flextable::compose(
            ft,
            i = i_row,
            j = j_col,
            value = flextable::as_paragraph(
              flextable::as_chunk(hit[2], props = normal_prop),
              flextable::as_chunk(hit[3], props = super_prop)
            ),
            part = "body"
          )
        }
      }
    }
    ft
  }

  build_ft <- function(df) {
    ft <- flextable::flextable(df)
    ft <- flextable::border_remove(ft)
    ft <- flextable::hline_top(ft, part = "all", border = officer::fp_border(color = "black", width = hline.top.table))
    ft <- flextable::hline_bottom(ft, part = "header", border = officer::fp_border(color = "black", width = hline.middle.table))
    ft <- flextable::hline_bottom(ft, part = "all", border = officer::fp_border(color = "black", width = hline.bottom.table))
    ft <- flextable::font(ft, part = "all", fontname = text.table.content.family)
    ft <- flextable::fontsize(ft, part = "all", size = text.table.content.size)
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
    ft <- flextable::align(ft, part = "header", align = "center")
    ft <- flextable::padding(ft, part = "all", padding = pad.table.cell)
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)

    p_cols <- colnames(df)[tolower(trimws(colnames(df))) %in% c("pvalue", "p_value", "p.value", "p", "p值")]
    if (length(p_cols) > 0) {
      ft <- flextable::set_header_labels(ft, values = stats::setNames(rep("P", length(p_cols)), p_cols))
      ft <- flextable::italic(ft, j = p_cols, italic = TRUE, part = "header")
    }
    ft
  }

  get_method_tables <- function(lang = c("cn", "en")) {
    lang <- match.arg(lang)
    tm <- if (!is.null(report_text)) report_text$TableMethod else NULL
    if (is.null(tm) || length(tm) == 0) return(list())
    nm <- names(tm)
    if (is.null(nm)) nm <- paste0("Table ", seq_along(tm))
    if (lang == "cn") {
      idx <- grepl("^表|统计学方法", nm)
      if (!any(idx)) idx <- seq_along(tm) == 1
    } else {
      idx <- grepl("^Table|Statistical", nm, ignore.case = TRUE)
      if (!any(idx)) idx <- seq_along(tm) == length(tm)
    }
    out <- tm[idx]
    names(out) <- nm[idx]
    out
  }

  get_table_names <- function() {
    table_names <- names(final_data_list)
    if (is.null(table_names) || length(table_names) == 0) {
      table_names <- paste0("Table_", seq_along(final_data_list))
      names(final_data_list) <<- table_names
    }
    table_names
  }

  render_method_section <- function(doc, lang = c("cn", "en")) {
    lang <- match.arg(lang)
    if (!isTRUE(use_auto_report)) return(doc)
    meth <- report_text$Methods
    doc <- add_heading(doc, if (lang == "cn") "统计学方法" else "Statistical Methods", level = 1, spacer_after = TRUE)
    doc <- add_paragraph_block(
      doc,
      if (lang == "cn") meth$chinese else meth$english,
      family = text.intro.family,
      size = text.intro.size,
      spacing = line.intro.spacing,
      bold = FALSE,
      spacer_after_block = TRUE
    )

    if (isTRUE(show_stat_table)) {
      mt <- get_method_tables(lang)
      if (length(mt) > 0) {
        for (i in seq_along(mt)) {
          doc <- add_table_caption(doc, names(mt)[i], spacer_after = TRUE)
          ft_tmp <- build_ft(as.data.frame(mt[[i]], stringsAsFactors = FALSE))
          ft_tmp <- flextable::align(ft_tmp, part = "body", align = "center")
          doc <- flextable::body_add_flextable(doc, ft_tmp)
          doc <- add_spacer(doc)
        }
      }
    }
    doc
  }

  render_results_section <- function(doc, lang = c("cn", "en"), include_tables = TRUE) {
    lang <- match.arg(lang)
    table_names <- get_table_names()

    if (isTRUE(use_auto_report)) {
      doc <- add_heading(doc, if (lang == "cn") "结果概述" else "Results Summary", level = 1, spacer_after = TRUE)
    }

    for (tabid in seq_along(table_names)) {
      nm <- table_names[tabid]
      if (tabid > 1) doc <- officer::body_add_break(doc)

      doc <- add_heading(
        doc,
        if (lang == "cn") paste0("按", nm, "分组的结果") else paste0("Results for ", nm),
        level = 2,
        spacer_after = TRUE
      )

      if (isTRUE(use_auto_report) && !is.null(report_text$Results[[nm]])) {
        sec <- report_text$Results[[nm]]
        doc <- add_paragraph_block(
          doc,
          if (lang == "cn") sec$chinese else sec$english,
          family = text.textcom.family,
          size = text.textcom.size,
          spacing = line.textcom.spacing,
          bold = FALSE,
          spacer_after_block = TRUE
        )
      }

      if (isTRUE(include_tables)) {
        df <- as.data.frame(final_data_list[[nm]]$cross_table, stringsAsFactors = FALSE)
        doc <- add_table_caption(
          doc,
          if (lang == "cn") paste0("表 ", tabid, ". ", nm) else paste0("Table ", tabid, ". ", nm),
          spacer_after = TRUE
        )
        ft <- build_ft(df)
        ft <- format_ft_by_rowid(ft, df)
        ft <- apply_pairwise_mark_superscript(ft, df)
        doc <- flextable::body_add_flextable(doc, ft)

        if (isTRUE(use_auto_report) && !is.null(report_text$TableNotes[[nm]])) {
          tn <- report_text$TableNotes[[nm]]
          doc <- add_paragraph_block(
            doc,
            if (lang == "cn") tn$chinese else tn$english,
            family = text.table.note.family,
            size = text.table.note.size,
            spacing = line.table.note.spacing,
            bold = FALSE,
            spacer_after_block = FALSE
          )
        }

        if (!is.null(table_comments)) {
          cm <- if (!is.null(names(table_comments)) && nm %in% names(table_comments)) table_comments[[nm]] else NULL
          if (!is.null(cm)) {
            doc <- add_paragraph_block(
              doc,
              cm,
              family = text.table.note.family,
              size = text.table.note.size,
              spacing = line.table.note.spacing,
              bold = FALSE,
              spacer_after_block = FALSE
            )
          }
        }
      } else {
        doc <- add_paragraph(
          doc,
          "Detailed tables are presented in the Chinese section above.",
          family = text.table.note.family,
          size = text.table.note.size,
          spacing = line.table.note.spacing,
          bold = FALSE,
          spacer_after = FALSE
        )
      }
    }
    doc
  }

  if (identical(bilingual_layout, "legacy")) {
    # Backward-compatible, compact behavior: still uses the new title but keeps all tables only once.
    doc <- add_title(doc, "Statistical Report / 统计分析报告")
    if (nzchar(trimws(as.character(introduction)))) {
      doc <- add_paragraph(doc, introduction, family = text.intro.family, size = text.intro.size, spacing = line.intro.spacing)
    }
    doc <- render_method_section(doc, "cn")
    if (use_auto_report && length(report_text$Methods$english) > 0) {
      doc <- add_paragraph_block(doc, report_text$Methods$english, family = text.intro.family, size = text.intro.size, spacing = line.intro.spacing)
    }
    doc <- officer::body_add_break(doc)
    doc <- render_results_section(doc, "cn", include_tables = TRUE)
  } else {
    doc <- add_title(doc, "统计分析报告")
    if (nzchar(trimws(as.character(introduction)))) {
      doc <- add_paragraph(doc, introduction, family = text.intro.family, size = text.intro.size, spacing = line.intro.spacing)
    }
    doc <- render_method_section(doc, "cn")
    doc <- officer::body_add_break(doc)
    doc <- render_results_section(doc, "cn", include_tables = TRUE)

    has_english <- isTRUE(use_auto_report) && (
      length(normalize_text_block(report_text$Methods$english)) > 0 ||
        any(vapply(report_text$Results, function(x) length(normalize_text_block(x$english)) > 0, logical(1)))
    )

    if (isTRUE(has_english)) {
      doc <- officer::body_add_break(doc)
      doc <- add_title(doc, "Statistical Report")
      doc <- render_method_section(doc, "en")
      doc <- officer::body_add_break(doc)
      doc <- render_results_section(doc, "en", include_tables = isTRUE(repeat_tables_in_english))
    }
  }

  print(doc, target = output_path)
  message("Word report generated successfully: ", output_path)
  invisible(output_path)
}
