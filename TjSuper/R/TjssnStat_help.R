#' 查看 TjssnStat 支持的分析模块
#'
#' `TjssnStat_topics()` 用于列出当前 `TjssnStat()` 支持的服务器端
#' 统计分析模块及其对应的帮助文档主题。
#'
#' @return 一个数据框，包含 `Stat`、`Topic` 和 `Title`。
#'
#' @examples
#' TjssnStat_topics()
#'
#' @export
TjssnStat_topics <- function() {
  .tjssnstat_registry()
}


#' 打开 TjssnStat 指定 Stat 模块的参数帮助
#'
#' `TjssnStat_help()` 用于打开不同 `Stat` 参数对应的专门帮助文档。
#'
#' 例如：
#'
#' \preformatted{
#' TjssnStat_help("Super.Table")
#' }
#'
#' 会打开 `Stat = "Super.Table"` 的参数说明页。
#'
#' 如果不输入 `Stat`，则会列出当前已经注册的所有分析模块。
#'
#' @param Stat 字符串。要查看帮助的分析模块名称，例如 `"Super.Table"`。
#'   如果为 `NULL`、`"help"` 或 `"?"`，则列出所有已注册模块。
#' @param package 字符串。包名，默认 `"TjSuper"`。
#'
#' @return 不可见地返回帮助对象或模块注册表。
#'
#' @examples
#' TjssnStat_help()
#' \dontrun{
#' TjssnStat_help("Super.Table")
#' }
#'
#' @export
TjssnStat_help <- function(Stat = NULL, package = "TjSuper") {
  registry <- .tjssnstat_registry()

  if (is.null(Stat) || identical(Stat, "help") || identical(Stat, "?")) {
    print(registry, row.names = FALSE)
    message(
      "\n使用方法：\n",
      "  TjssnStat_help(\"Super.Table\")\n\n",
      "也可以使用：\n",
      "  help(TjssnStat)\n",
      "  help(\"TjssnStat-Super.Table\")\n"
    )
    return(invisible(registry))
  }

  if (!is.character(Stat) || length(Stat) != 1 || !nzchar(trimws(Stat))) {
    stop("`Stat` must be a single non-empty character string.", call. = FALSE)
  }

  Stat <- trimws(Stat)

  matched <- which(registry$Stat == Stat)

  if (length(matched) == 0) {
    matched <- which(tolower(registry$Stat) == tolower(Stat))
  }

  if (length(matched) == 0) {
    stop(
      "Unknown Stat: ", Stat, "\n\n",
      "当前可用的 Stat 包括：\n  - ",
      paste(registry$Stat, collapse = "\n  - "),
      "\n\n",
      "可先运行：\n",
      "  TjssnStat_help()\n",
      call. = FALSE
    )
  }

  topic <- registry$Topic[matched[1]]

  help_obj <- tryCatch(
    do.call(utils::help, list(topic = topic, package = package)),
    error = function(e) NULL
  )

  if (is.null(help_obj) || length(help_obj) == 0) {
    stop(
      "找不到帮助主题：", topic, "\n\n",
      "请确认：\n",
      "1. 文件 `R/help-Super.Table.R` 是否存在；\n",
      "2. 帮助页中是否包含 `@name ", topic, "`；\n",
      "3. 帮助页最后是否有一行 `NULL`；\n",
      "4. 是否已经运行 `devtools::document()`；\n",
      "5. 是否生成了 `man/", topic, ".Rd`；\n",
      "6. 如果你是从 GitHub 安装后测试，是否已经重新安装最新版。\n",
      call. = FALSE
    )
  }

  print(help_obj)
  invisible(help_obj)
}
