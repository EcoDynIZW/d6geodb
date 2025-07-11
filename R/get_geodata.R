#' get data of the database
#'
#' Get the data from the overview or directly set the name of the data.
#'
#' @param name name of the data, if null you can decide from the database itself.
#' @param download do you want the data to be downloaded?
#'
#' @export
#' @examples
#' \dontrun{
#' get_geodata()
#' }

get_geodata <- function(name = NULL, download = FALSE){

  con <- con_geodb()

  DBI::dbExecute(con, "SET search_path TO geodata")

  if(is.null(name)){
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

name <- DBI::dbGetQuery(con,
                   glue::glue("SELECT folder_name FROM geodata.metadata WHERE ",
                              col,
                              " = '",
                              value,
                              "'"))[,1][utils::menu(DBI::dbGetQuery(con,
                                                               glue::glue("SELECT folder_name FROM geodata.metadata WHERE ",
                                                                          col,
                                                                          " = '",
                                                                          value,
                                                                          "'"))[,1], title =  "select layer:")]

sub_name <- DBI::dbGetQuery(con,
                       glue::glue("SELECT sub_name FROM geodata.metadata WHERE folder_name = '",
                                  name,
                                  "'"))[,1]

if(stringr::str_detect(name, "tif")){

  data <- rpostgis::pgGetRast(con, sub_name)

  return(data)
}

if(stringr::str_detect(name, "gpkg")){

  data <- sf::st_read(con, sub_name)

  return(data)
}


} else {

  sub_name <- DBI::dbGetQuery(con,
                         glue::glue("SELECT sub_name FROM geodata.metadata WHERE folder_name = '",
                                    name,
                                    "'"))[,1]

  if(stringr::str_detect(name, "tif")){

    data <- rpostgis::pgGetRast(con, sub_name)

    if(download == TRUE){
      dir.create(paste(getwd(),
                       "data",
                       name,
                       sep = "/"))

      terra::writeRaster(data,
                         paste(getwd(),
                               "data",
                               name,
                               stringi::stri_replace_last_fixed(name, "_", "."),
                               sep = "/"))

      write.csv(dbGetQuery(con, glue::glue("SELECT * FROM metadata WHERE sub_name = '", sub_name,"'")),
                paste(getwd(),
                      "data",
                      name,
                      paste0("meta-data_", name, ".csv"),
                      sep = "/"),
                row.names = FALSE)
    }

    return(data)
  }

  if(stringr::str_detect(name, "gpkg")){

    data <- sf::st_read(con, sub_name)

    if(download == TRUE){
      dir.create(paste(getwd(),
                       "data",
                       name,
                       sep = "/"))

      sf::st_write(data,
                         paste(getwd(),
                               "data",
                               name,
                               stringi::stri_replace_last_fixed(name, "_", "."),
                               sep = "/"))

      write.csv(dbGetQuery(con, glue::glue("SELECT * FROM metadata WHERE sub_name = '", sub_name,"'")),
                paste(getwd(),
                      "data",
                      name,
                      paste0("meta-data_", name, ".csv"),
                      sep = "/"),
                row.names = FALSE)
    }

    return(data)

  }
}

return(data)


}
