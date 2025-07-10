#' overview of geodatabase
#'
#' Get the overview of the geodata in the database. You can decide which column you want to use for subsetting the data.
#' @param all see the whole meta data table
#' @export
#' @examples
#' \dontrun{
#' geodata_overview()
#' }

geodata_overview <- function(all = FALSE){

  con <- con_geodb()

if(all == TRUE){
  DBI::dbGetQuery(con, glue::glue("SELECT name FROM geodata.metadata"))
} else{

col <- c(DBI::dbListFields(con,
                           DBI::Id(schema = "geodata",
                  table = "metadata")))[utils::menu(c(DBI::dbListFields(con,
                                                                        DBI::Id(schema = "geodata",
                                                                      table = "metadata")),
                title = "select column"))]


# does not work so far
value <- DBI::dbGetQuery(con, glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))[,1][utils::menu(DBI::dbGetQuery(con, glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))[,1],title = glue::glue("select ", col, ":"))]

return(value)

  }
}









