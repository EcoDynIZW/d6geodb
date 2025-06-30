#' overview of geodatabase
#'
#' Get the overview of the geodata in the database. You can decide which column you want to use for subsetting the data.
#'
#' @export
#' @examples
#' \dontrun{
#' geodata_overview()
#' }

geodata_overview <- function(all = FALSE){

if(all == TRUE){
  dbGetQuery(con_geodb(), glue::glue("SELECT name FROM geodata.metadata"))
} else{

col <- c(dbListFields(con_geodb(),
               Id(schema = "geodata",
                  table = "metadata")))[utils::menu(c(dbListFields(con_geodb(),
                                                                   Id(schema = "geodata",
                                                                      table = "metadata")),
                title = "select column"))]


# does not work so far
value <- dbGetQuery(con_geodb(), glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))[,1][utils::menu(dbGetQuery(con_geodb(), glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM geodata.metadata
"))[,1],title = glue::glue("select ", col, ":"))]

return(value)

  }
}









