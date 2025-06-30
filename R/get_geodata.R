#' get data of the database
#'
#' Get the data from the overview or directly set the name of the data.
#'
#' @param name name of the data, if null you can decide from the database itself.
#'
#' @export
#' @examples
#' \dontrun{
#' get_geodata()
#' }


# DOES not work

get_geodata <- function(name = NULL){

  if(is.null(name)){
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

name <- dbGetQuery(con_geodb(),
                   glue::glue("SELECT folder_name FROM geodata.metadata WHERE ",
                              col,
                              " = '",
                              value,
                              "'"))[,1][utils::menu(dbGetQuery(con_geodb(),
                                                               glue::glue("SELECT folder_name FROM geodata.metadata WHERE ",
                                                                          col,
                                                                          " = '",
                                                                          value,
                                                                          "'"))[,1], title =  "select layer:")]

sub_name <- dbGetQuery(con_geodb(),
                       glue::glue("SELECT sub_name FROM geodata.metadata WHERE folder_name = '",
                                  name,
                                  "'"))[,1]

if(stringr::str_detect(name, "tif")){

  data <- rpostgis::pgGetRast(con_geodb(), glue::glue("geodata.",  sub_name))

  return(data)
}

if(stringr::str_detect(name, "gpkg")){

  data <- sf::st_read(con_geodb(), glue::glue("geodata.",  sub_name))

  return(data)
}


} else {

  sub_name <- dbGetQuery(con_geodb(),
                         glue::glue("SELECT sub_name FROM geodata.metadata WHERE folder_name = '",
                                    name,
                                    "'"))[,1]

  if(stringr::str_detect(name, "tif")){

    data <- rpostgis::pgGetRast(con_geodb(), glue::glue("geodata.", sub_name))

    return(data)
  }

  if(stringr::str_detect(name, "gpkg")){

    data <- sf::st_read(con_geodb(), glue::glue("geodata.", sub_name))

    return(data)
  }
}

return(data)

}
