




TJ_float <- function(x,
                     digits = 2,
                     unit = "",
                     as_string = FALSE) {
  if (is.null(x) || (is.vector(x) && length(x) == 0)) {
    warning("输入x为空（NULL或空向量），返回NULL")
    return(NULL)
  }


  if (!is.numeric(digits) ||
      length(digits) != 1 ||
      digits < 0 || digits != as.integer(digits)) {
    warning("参数digits必须是单个非负整数（如0,1,2），返回NULL")
    return(NULL)
  }


  if (!is.character(unit) || length(unit) != 1) {
    warning("参数unit必须是单个字符串（如\"kg\"），返回NULL")
    return(NULL)
  }


  if (!is.numeric(x)) {
    x_num <- suppressWarnings(as.numeric(x))
    if (all(is.na(x_num))) {
      warning("输入x无法转换为数值型，返回NULL")
      return(NULL)
    } else {
      warning("输入x为非数值型，已尝试转换为数值型")
      x <- x_num
    }
  }


  na_indices <- is.na(x)
  x_formatted <- character(length(x))


  fmt <- paste0("%.", digits, "f")
  x_formatted[!na_indices] <- sprintf(fmt, x[!na_indices])
  x_formatted[na_indices] <- NA_character_


  if (unit != "") {
    x_formatted[!na_indices] <- paste0(x_formatted[!na_indices], unit)
  }


  if (!as_string) {
    if (unit != "") {
      warning("unit非空时自动返回字符串（数值型无法保留单位）")
    } else {
      x_formatted <- as.numeric(x_formatted)
    }
  }


  if (is.matrix(x) || is.array(x)) {
    dim(x_formatted) <- dim(x)
    dimnames(x_formatted) <- dimnames(x)
  }

  return(x_formatted)
}
