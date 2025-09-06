#' 创建事件相关参数列表
#'
#' 该函数用于创建一个包含事件相关参数的列表，通常用于生存分析、事件史分析等场景，
#' 集中管理结局变量、事件指示、时间变量、参照组和竞争风险等信息。
#'
#' @param y 向量，可选参数，默认值为NA。主要结局变量，通常表示事件是否发生或事件类型。
#' @param event 向量，可选参数，默认值为NA。事件指示变量，用于标记事件是否发生（如1表示发生，0表示未发生）。
#' @param time 向量，可选参数，默认值为NA。时间变量，记录从起始点到事件发生或截尾的时间。
#' @param ref 标量或字符，可选参数，默认值为NA。参照组标识，指定分析中的参照类别。
#' @param cmprsk 向量或逻辑型，可选参数，默认值为NA。竞争风险指示变量，若为逻辑型则标记是否存在竞争风险，
#'   若为向量则表示不同类型的竞争风险。
#'
#' @return 包含以下元素的列表：
#' \item{y}{输入的结局变量}
#' \item{event}{输入的事件指示变量}
#' \item{time}{输入的时间变量}
#' \item{ref}{输入的参照组标识}
#' \item{cmprsk}{输入的竞争风险信息}
#'
#' @examples
#' # 示例1：创建基本的事件参数列表
#' event_obj <- Event(
#'   y = c(1, 0, 1, 0, 1),
#'   event = c(1, 0, 1, 0, 1),
#'   time = c(12, 24, 36, 18, 6)
#' )
#'
#' # 示例2：包含参照组和竞争风险的事件参数
#' event_obj2 <- Event(
#'   y = c(1, 2, 1, 3, 2),
#'   event = c(1, 1, 1, 1, 1),
#'   time = c(5, 10, 15, 20, 25),
#'   ref = 1,
#'   cmprsk = c(FALSE, TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' @export
Event <- function(y=NA,event=NA,time=NA,ref=NA,cmprsk=NA){
  A <- list(y=y,
            event=event,
            time=time,
            ref=ref,
            cmprsk=cmprsk)
  return(A)
}
