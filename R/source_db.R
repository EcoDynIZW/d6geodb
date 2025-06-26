#' read database credentials
#'
#' @param path the path to source file
#' @export
#' @examples
#' \dontrun{
#' source_db()
#' }


source_db <- function(path){
  source(file = path)
}
