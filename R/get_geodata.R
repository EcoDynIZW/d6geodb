#' get data of the database
#'
#' Get the data from the overview or directly set the name of the data.
#'
#' @param name name of the data, if null you can decide from the database itself.
#' @param extent works for raster data only. You can use a sf object, SpatVector object, or numeric to get a subset of this extent of the object.
#'
#' @export
#' @examples
#' \dontrun{
#' get_geodata()
#' }

get_geodata <- function(name = NULL, extent = NULL){

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

    }

    if(stringr::str_detect(name, "gpkg")){

      data <- sf::st_read(con, sub_name)

    }


  } else {

    sub_name <- DBI::dbGetQuery(con,
                                glue::glue("SELECT sub_name FROM geodata.metadata WHERE folder_name = '",
                                           name,
                                           "'"))[,1]

    if(stringr::str_detect(name, "tif")){
      if(is.null(extent)){
      data <- rpostgis::pgGetRast(conn = con, name = sub_name)
      } else{
        data <- rpostgis::pgGetRast(conn = con, name = sub_name, boundary = extent)
      }
    }

    if(stringr::str_detect(name, "gpkg")){

      data <- sf::st_read(con, sub_name)
    }
  }

  download_meta <- c(TRUE, FALSE)[utils::menu(c("yes", "no"),title = "do you want to download the meta-data?")]
  download_data <- c(TRUE, FALSE)[utils::menu(c("yes", "no"),title = "do you want to download the data?")]

  if(download_meta == TRUE){
    if(!dir.exists(paste(getwd(),
                         "data",
                         name,
                         sep = "/"))){
      dir.create(paste(getwd(),
                       "data",
                       name,
                       sep = "/"))}

    meta <- glue::glue("SELECT * FROM metadata WHERE sub_name = '", sub_name,"'")
    meta$date_of_download_db <- Sys.Date()

    if(!is.null(extent)){
      meta$extent_used <- "yes"
    }

    write.csv(DBI::dbGetQuery(con, meta),
              paste(getwd(),
                    "data",
                    name,
                    paste0("meta-data_", name, ".csv"),
                    sep = "/"),
              row.names = FALSE)

    if(download_data == TRUE){
    if(stringr::str_detect(name, "gpkg")){


      sf::st_write(data,
                   paste(getwd(),
                         "data",
                         name,
                         stringi::stri_replace_last_fixed(name, "_", "."),
                         sep = "/"), delete_layer = TRUE)

    }

    if(stringr::str_detect(name, "tif")){

      terra::writeRaster(data,
                         paste(getwd(),
                               "data",
                               name,
                               stringi::stri_replace_last_fixed(name, "_", "."),
                               sep = "/"), overwrite = TRUE)

    }
    }
  }

  return(data)
}




