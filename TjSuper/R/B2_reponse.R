
TJanalysis.response <- function(file) {

  response <- httr::POST(
    url = "https://tjssn.cpolar.top/upload_R",
    body = list(uuid = get_device_id2(), file = httr::upload_file(file)),
    encode = "multipart"
  )

  if (httr::status_code(response) == 200) {
    temp_file <- tempfile(fileext = ".rda")
    on.exit(unlink(temp_file, force = TRUE))  

    writeBin(httr::content(response, "raw"), temp_file)
    loaded_env <- new.env()
    load(temp_file, envir = loaded_env)

    loaded_objects <- ls(loaded_env)
    if (length(loaded_objects) == 0) {
      stop("")
    }

    if (length(loaded_objects) > 1) {
      warning(
        "(",
        paste(loaded_objects, collapse = ", "),
        ")"
      )
    }
    return(get(loaded_objects[1], envir = loaded_env))

  } else {
    error_msg <- paste("", httr::status_code(response), "\n")
    error_msg <- paste0(error_msg, " ", httr::content(response, "text"), "\n")
    stop(error_msg)
  }
}
