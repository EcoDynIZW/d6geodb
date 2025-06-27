#' overview of of geodatabes
#'
#' @param name complete name of data file
#' @param region region of file
#' @param  port
#' @param db_user user_name
#' @param column get column names
#' @export
#' @examples
#' \dontrun{
#' con_geodb()
#' }


con_geodb <- function(){
  dbConnect(RPostgres::Postgres(),
            dbname = db,
            host=host_db,
            port=db_port,
            user=db_user,
            password=db_password)

}

col <- c(dbListFields(con_geodb(),
               Id(schema = "geodata",
                  table = "metadata")))[utils::menu(c(dbListFields(con_geodb(),
                                                                   Id(schema = "geodata",
                                                                      table = "metadata")),
                title = "select column"))]


value <- c(as.vector(dbGetQuery(con_geodb(), glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))))[utils::menu(as.vector(dbGetQuery(con_geodb(), glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))),title = glue::glue("select ", col))]


dbGetQuery(con, glue::glue("SELECT name FROM metadata WHERE ",col," = '",,"'"))


dbListFields(con_geodb(), Id(schema = "geodata", table = "metadata"))




