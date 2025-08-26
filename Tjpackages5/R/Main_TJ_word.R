#' GBTM模型统计分析报告Word生成工具
#'
#' 从数据列表或模型对象中提取信息，生成标准化Word格式统计分析报告，支持报告标题、正文、表格前置说明、
#' 三线表格式表格、表注的完整内容构建，并提供灵活的格式自定义功能（字体类型、字号、行距、表格内边距）。
#' 函数支持参数优先级控制（用户自定义参数优先于模型对象默认参数），自动生成符合学术规范的三线表，
#' 最终在指定路径输出可直接使用的Word文档，并在控制台提示保存路径，便于后续报告整理与分享。
#'
#' @section 重要提示:
#' \strong{⚠️} 函数参数存在明确优先级关系：用户手动传入的`table_comments`/`introduction`/`text_comments`
#' 优先于`model_list`中提取的默认参数；若未传入`data_list`，将从`model_list`的`result$table$model.res`提取表格数据，
#' 二者均未提供时会报错。生成的表格默认采用学术规范三线表格式，若需调整表格边框样式，需手动修改函数内表格处理逻辑。
#' 请确保输出路径（`output_path`）具备写入权限，否则会导致Word文档保存失败。
#'
#' @param model_list 列表，可选参数。包含表格数据和默认文本参数的模型对象，需满足特定结构：
#'   - `result$table$model.res`：存储表格数据的列表（格式同`data_list`）；
#'   - `result$output.infor$my_annotations`：默认表注（格式同`table_comments`）；
#'   - `result$output.infor$introduction`：默认正文（格式同`introduction`）；
#'   - `result$output.infor$text_comments`：默认表格前置说明（格式同`text_comments`）。
#'   若未手动传入`data_list`/`table_comments`/`introduction`/`text_comments`，函数将从该对象提取默认值。
#' @param data_list 列表，可选参数。存储待插入Word表格的列表，每个元素为一个数据框（data.frame），
#'   列表名称即为表格名称（将显示在表格标题中），格式为`list("表格1名称" = 数据框1, "表格2名称" = 数据框2)`。
#'   若与`model_list`同时提供，`data_list`优先级低于`model_list$result$table$model.res`（即优先使用模型对象中的表格数据）。
#' @param output_path 字符型，必填参数。Word文档的输出路径（含文件名），格式需为`.docx`，例如`"D:/GBTM报告.docx"`。
#' @param table_comments 列表，可选参数。存储表注内容的命名列表，列表名称需与`data_list`/`model_list`中的表格名称一致，
#'   格式为`list("表格1名称" = "表1注释放置", "表格2名称" = "表2注释放置")`。用户手动传入时，优先级高于`model_list`中的默认表注。
#' @param introduction 字符型，可选参数。报告正文内容，支持包含统计分析背景、数据来源、分析方法等描述性文本，
#'   函数会自动为正文添加首行缩进。用户手动传入时，优先级高于`model_list`中的默认正文。
#' @param text_comments 列表，可选参数。存储表格前置说明的命名列表，列表名称需与`data_list`/`model_list`中的表格名称一致，
#'   用于在对应表格标题前添加说明文本（如表格核心内容、数据含义等），格式为`list("表格1名称" = "表1前置说明", "表格2名称" = "表2前置说明")`。
#'   用户手动传入时，优先级高于`model_list`中的默认表格前置说明。
#'
#' @param text.title.size 数值型，可选参数。报告标题的字号，默认值为14（单位：磅），对应参数含义为“文本.报告标题.字号”。
#' @param text.intro.size 数值型，可选参数。报告正文的字号，默认值为12（单位：磅），对应参数含义为“文本.正文.字号”。
#' @param text.textcom.size 数值型，可选参数。表格前置说明的字号，默认值为12（单位：磅），对应参数含义为“文本.表格前置说明.字号”。
#' @param text.table.caption.size 数值型，可选参数。表格标题的字号，默认值为12（单位：磅），对应参数含义为“文本.表格标题.字号”。
#' @param text.table.content.size 数值型，可选参数。表格内容（含表头）的字号，默认值为11（单位：磅），对应参数含义为“文本.表格内容.字号”。
#' @param text.table.note.size 数值型，可选参数。表注的字号，默认值为10（单位：磅），对应参数含义为“文本.表注.字号”。
#'
#' @param line.title.spacing 数值型，可选参数。报告标题的行距倍数，默认值为1.0（单倍行距），对应参数含义为“行距.报告标题.行距”。
#' @param line.intro.spacing 数值型，可选参数。报告正文的行距倍数，默认值为1.5（1.5倍行距），对应参数含义为“行距.正文.行距”。
#' @param line.textcom.spacing 数值型，可选参数。表格前置说明的行距倍数，默认值为1.5（1.5倍行距），对应参数含义为“行距.表格前置说明.行距”。
#' @param line.caption.spacing 数值型，可选参数。表格标题的行距倍数，默认值为1.0（单倍行距），对应参数含义为“行距.表格标题.行距”。
#' @param line.table.note.spacing 数值型，可选参数。表注的行距倍数，默认值为1.5（1.5倍行距），对应参数含义为“行距.表注.行距”。
#'
#' @param text.title.family 字符型，可选参数。报告标题的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.报告标题.字体”。
#' @param text.intro.family 字符型，可选参数。报告正文的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.正文.字体”。
#' @param text.textcom.family 字符型，可选参数。表格前置说明的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.表格前置说明.字体”。
#' @param text.table.caption.family 字符型，可选参数。表格标题的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.表格标题.字体”。
#' @param text.table.content.family 字符型，可选参数。表格内容的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.表格内容.字体”。
#' @param text.table.note.family 字符型，可选参数。表注的字体类型，默认值为`"Times New Roman"`，对应参数含义为“文本.表注.字体”。
#'
#' @param pad.table.cell 数值型，可选参数。表格单元格的内边距（即单元格内容与边框的距离），默认值为1（单位：磅），
#'   对应参数含义为“内边距.表格单元格.内边距”，值越小表格越紧凑。
#'
#' @details
#' 函数处理流程与参数优先级逻辑：\cr
#' 1. 数据与文本参数提取：先尝试从`model_list`提取表格数据（`result$table$model.res`）和默认文本（正文/前置说明/表注），
#'    若用户手动传入`data_list`/`table_comments`/`introduction`/`text_comments`，则覆盖默认值；\cr
#' 2. 报告结构构建：按“报告标题 → 正文 → （分页 → 表格前置说明 → 表格标题 → 三线表 → 表注）×N”的顺序生成内容，
#'    其中N为表格数量，从第二个表格开始自动分页；\cr
#' 3. 格式应用：标题默认居中加粗，正文默认首行缩进，表格默认三线表（上粗线1.5pt、中细线0.75pt、下粗线1.5pt），
#'    所有格式参数（字体/字号/行距/内边距）均通过“见名知意”的参数控制，默认值符合学术报告规范；\cr
#' 4. 文档输出：在指定路径生成Word文档，控制台打印保存路径，无显式返回值。
#'
#' @note
#' 1. 若需修改表格边框样式（如调整线条粗细、颜色），需在函数内`生成表格`模块修改`fp_border`的`width`和`color`参数；\cr
#' 2. 正文首行缩进由函数自动添加（通过`paste0("\t", final_introduction)`实现），无需在`introduction`参数中手动添加空格；\cr
#' 3. 表格名称需避免包含特殊字符（如`/ \ : * ? " < > |`），否则可能导致表格标题显示异常或文档保存失败；\cr
#' 4. 若`model_list`结构不符合要求（如缺少`result$table$model.res`），函数将优先尝试使用`data_list`，二者均无则报错。
#'
#' @author
#' 开发团队:统计碎碎念团队;\cr
#' 联系方式:VX:Tongjissn;\cr
#' 官方平台-公众号:统计碎碎念
#'
#' @examples
#' TJword(
#'   model_list = GBTM_Model2,
#'   output_path = "A23.docx",
#'   introduction="本报告呈现了基于潜在类别增长模型(GBTM)的统计分析结果。分析数据来源于标准化收集的样本群体(n=500)，通过模型拟合得到3个不同类别的增长轨迹特征(低增长组、中增长组、高增长组)。报告包含模型拟合优度指标、类别划分结果及关键参数估计值，可为后续干预策略制定提供参考依据。所有分析均采用p < 0.05作为统计显著性水平，使用R软件(flexmix包)完成数据处理与模型构建。",
#'   text_comments=c("组轨迹模型的拟合效果评价指标1"="本研究结果显示, OCC AVEPP 等指标均符合建模标准，进一步查看Table2 组轨迹模型的拟合效果评价指标1。")
#' )
#' TJword(
#'   data_list = GBTM_Model2$result$table$model.res,
#'   output_path = "DATAA23.docx"
#' )

# ===================== 2. 主函数 =====================
TJword <- function(
    # ---------------------- 核心数据参数 ----------------------
    model_list = NULL,
    data_list = NULL,
    output_path,
    table_comments = NULL,  # 表注内容（自定义优先）
    introduction = "",      # 正文内容（自定义优先）
    text_comments = NULL,   # 表格前置说明内容（自定义优先）

    # ---------------------- 字体大小参数：text.位置.功能 ----------------------
    text.title.size = 14,          # 文本.报告标题.字号（默认14号）
    text.intro.size = 12,          # 文本.正文.字号（默认12号）
    text.textcom.size = 12,        # 文本.表格前置说明.字号（默认12号）
    text.table.caption.size = 12,  # 文本.表格标题.字号（默认12号）
    text.table.content.size = 11,  # 文本.表格内容.字号（默认11号）
    text.table.note.size = 10,     # 文本.表注.字号（默认10号）

    # ---------------------- 行距参数：line.位置.功能 ----------------------
    line.title.spacing = 1.0,      # 行距.报告标题.行距（默认1.0倍）
    line.intro.spacing = 1.5,      # 行距.正文.行距（默认1.5倍）
    line.textcom.spacing = 1.5,    # 行距.表格前置说明.行距（默认1.5倍）
    line.caption.spacing = 1.0,    # 行距.表格标题.行距（默认1.0倍）
    line.table.note.spacing = 1.5, # 行距.表注.行距（默认1.5倍）

    # ---------------------- 字体类型参数：text.位置.字体 ----------------------
    text.title.family = "Times New Roman",          # 文本.报告标题.字体（默认Times）
    text.intro.family = "Times New Roman",          # 文本.正文.字体（默认Times）
    text.textcom.family = "Times New Roman",        # 文本.表格前置说明.字体（默认Times）
    text.table.caption.family = "Times New Roman",  # 文本.表格标题.字体（默认Times）
    text.table.content.family = "Times New Roman",  # 文本.表格内容.字体（默认Times）
    text.table.note.family = "Times New Roman",     # 文本.表注.字体（默认Times）

    # ---------------------- 表格内边距参数：pad.位置.功能 ----------------------
    pad.table.cell = 1  # 内边距.表格单元格.内边距（默认1pt）
) {
  # 1. 初始化纯净文档
  doc <- read_docx()

  # 2. 数据处理：参数优先级逻辑
  model_defaults <- list()
  if (!is.null(model_list) && "result" %in% names(model_list) && "output.infor" %in% names(model_list[["result"]])) {
    output_infor <- model_list[["result"]][["output.infor"]]
    model_defaults$table_comments <- if ("my_annotations" %in% names(output_infor)) output_infor[["my_annotations"]] else NULL
    model_defaults$introduction <- if ("introduction" %in% names(output_infor)) output_infor[["introduction"]] else ""
    model_defaults$text_comments <- if ("text_comments" %in% names(output_infor)) output_infor[["text_comments"]] else NULL
  }

  # 确定最终参数
  final_table_comments <- if (!is.null(table_comments)) table_comments else model_defaults$table_comments
  final_introduction <- if (introduction != "") introduction else model_defaults$introduction
  final_text_comments <- if (!is.null(text_comments)) text_comments else model_defaults$text_comments
  final_data_list <- if (!is.null(model_list) && "result" %in% names(model_list) && "table" %in% names(model_list[["result"]]) && "model.res" %in% names(model_list[["result"]][["table"]])) {
    model_list[["result"]][["table"]][["model.res"]]
  } else if (!is.null(data_list)) {
    data_list
  } else {
    stop("需提供data_list(表格数据)或含表格数据的model_list(模型对象)")
  }

  # 3. 添加报告标题
  doc <- body_add_fpar(
    x = doc,
    value = fpar(
      ftext(
        "统计碎碎念",
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
  ) %>%
    body_add_par(x = ., value = "", style = "Normal")

  # 4. 添加正文
  doc <- body_add_fpar(
    x = doc,
    value = fpar(
      ftext(
        paste0("\t", final_introduction),
        fp_text(
          font.family = text.intro.family,
          font.size = text.intro.size
        )
      ),
      fp_p = fp_par(
        line_spacing = line.intro.spacing
      )
    )
  ) %>% body_add_break(x = .)

  # 5. 表格处理核心逻辑（修复链式调用：补充%>%，传递表格对象x）
  tabid <- 1
  table_names <- names(final_data_list)

  for (i in seq_along(table_names)) {
    table_name <- table_names[i]
    df <- final_data_list[[table_name]]

    # 5.1 分页
    if (i > 1) doc <- body_add_break(x = doc)

    # 5.2 表格前置说明
    if (!is.null(final_text_comments) && table_name %in% names(final_text_comments)) {
      doc <- body_add_fpar(
        x = doc,
        value = fpar(
          ftext(
            paste0("\t", final_text_comments[[table_name]]),
            fp_text(
              font.family = text.textcom.family,
              font.size = text.textcom.size
            )
          ),
          fp_p = fp_par(
            line_spacing = line.textcom.spacing
          )
        )
      ) %>%
        body_add_par(x = ., value = "", style = "Normal")
    }

    # 5.3 表格标题
    caption_text <- paste0("Table ", tabid, ". ", table_name)
    doc <- body_add_fpar(
      x = doc,
      value = fpar(
        ftext(
          caption_text,
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
    ) %>% body_add_par(x = ., value = "", style = "Normal")

    # 5.4 生成表格（关键修复：用%>%连接每个函数，传递表格对象x）
    ft <- flextable(df) %>%
      border_remove() %>%
      hline_top(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
      hline_bottom(part = "header", border = fp_border(color = "black", width = 0.75)) %>%
      hline_bottom(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
      # 修复：补充%>%，让表格对象传递给font函数（x参数自动填充）
      font(part = "all", fontname = text.table.content.family) %>%
      # 修复：补充%>%，让表格对象传递给fontsize函数（x参数自动填充）
      fontsize(part = "all", size = text.table.content.size) %>%
      bold(part = "header", bold = TRUE) %>%
      align(part = "header", align = "center") %>%
      align(part = "body", align = "left") %>%
      # 修复：补充%>%，让表格对象传递给padding函数（x参数自动填充）
      padding(part = "all", padding = pad.table.cell) %>%
      set_table_properties(layout = "autofit", width = 1)

    # 5.5 添加表格到文档
    doc <- body_add_flextable(x = doc, value = ft)
    tabid <- tabid + 1

    # 5.6 表注
    if (!is.null(final_table_comments) && table_name %in% names(final_table_comments)) {
      doc <- body_add_fpar(
        x = doc,
        value = fpar(
          ftext(
            final_table_comments[[table_name]],
            fp_text(
              font.family = text.table.note.family,
              font.size = text.table.note.size
            )
          ),
          fp_p = fp_par(
            line_spacing = line.table.note.spacing
          )
        )
      ) %>% body_add_par(x = ., value = "", style = "Normal")
    }
  }

  # 6. 保存文档
  print(doc, target = output_path)
  message("Word save path：", normalizePath(output_path))
}

