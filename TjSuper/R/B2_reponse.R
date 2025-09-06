# 修改后的响应处理函数，直接返回结果而非全局赋值
TJanalysis.response <- function(file) {
  # 发送POST请求
  response <- httr::POST(
    url = "https://tjssn.cpolar.top/upload_R",
    body = list(uuid = get_device_id2(), file = httr::upload_file(file)),
    encode = "multipart"
  )

  # 处理响应
  if (httr::status_code(response) == 200) {
    temp_file <- tempfile(fileext = ".rda")
    on.exit(unlink(temp_file, force = TRUE))  # 确保临时文件被清理

    # 写入并加载临时文件
    writeBin(httr::content(response, "raw"), temp_file)
    loaded_env <- new.env()
    load(temp_file, envir = loaded_env)

    # 获取加载的对象
    loaded_objects <- ls(loaded_env)
    if (length(loaded_objects) == 0) {
      stop("从返回的RDA文件中未加载到任何对象")
    }

    # 如果有多个对象，给出警告并返回第一个
    if (length(loaded_objects) > 1) {
      warning(
        "返回的RDA文件包含多个对象(",
        paste(loaded_objects, collapse = ", "),
        ")，将仅返回第一个对象"
      )
    }

    # 返回结果对象
    return(get(loaded_objects[1], envir = loaded_env))

  } else {
    # 错误处理
    error_msg <- paste("请求错误，状态码:", httr::status_code(response), "\n")
    error_msg <- paste0(error_msg, "错误信息: ", httr::content(response, "text"), "\n")
    stop(error_msg)
  }
}
