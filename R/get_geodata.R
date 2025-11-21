#' get data of the database
#'
#' Get the data from the overview or directly set the name of the data.
#'
#' @param name name of the data, if null you can decide from the database itself.
#' @param extent please insert a spatRaster, spatVector or sf Object.
#'
#' @export
#' @examples
#' \dontrun{
#' get_geodata()
#' }

get_geodata <- function(name = NULL, extent = NULL){

  if(exists("con") && !is.null(con)) {
    con <- get("con", envir = .GlobalEnv)
    RPostgres::dbIsValid(con)
  } else {
    con <- con_geodb()
  }

  if(is.null(name)){
    fields <- DBI::dbListFields(con,
                                DBI::Id(schema = "envdata",
                                        table = "metadata"))

    col <- c(fields[fields %in% c("name", "type", "region", "year_of_data")])[utils::menu(c(fields[fields %in% c("name", "type", "region", "year_of_data")]),
                                                                           title = "select column")]


    # does not work so far
    value <- DBI::dbGetQuery(con, glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM envdata.metadata
"))[,1][utils::menu(DBI::dbGetQuery(con, glue::glue("
  SELECT DISTINCT {'",col,"'}
  FROM envdata.metadata
"))[,1],title = glue::glue("select ", col, ":"))]

    name <- DBI::dbGetQuery(con,
                            glue::glue("SELECT folder_name FROM envdata.metadata WHERE ",
                                       col,
                                       " = '",
                                       value,
                                       "'"))[,1][utils::menu(DBI::dbGetQuery(con,
                                                                             glue::glue("SELECT folder_name FROM envdata.metadata WHERE ",
                                                                                        col,
                                                                                        " = '",
                                                                                        value,
                                                                                        "'"))[,1], title =  "select layer:")]

    sub_name <- DBI::dbGetQuery(con,
                                glue::glue("SELECT sub_name FROM envdata.metadata WHERE folder_name = '",
                                           name,
                                           "'"))[,1]

    meta <- DBI::dbGetQuery(con,glue::glue("SELECT * FROM metadata WHERE sub_name = '", sub_name,"'"))

    if(!is.null(extent)){
      if(class(extent)[1] == "sf" | class(extent)[1] == "SpatVector"){
        ext <- as.numeric(sf::st_bbox(sf::st_transform(sf::st_as_sf(extent), meta$crs)))
      }

      if(class(extent)[1] == "SpatRaster"){
        ext_ras <- terra::ext(terra::project(extent, meta$crs))
        ext <- c(as.numeric(ext_ras[1]), as.numeric(ext_ras[3]),
                 as.numeric(ext_ras[2]), as.numeric(ext_ras[4]))
      }

    }


    if(stringr::str_detect(name, "tif")){
      if(is.null(extent)){
        data <- rpostgis::pgGetRast(conn = con, name = sub_name)
      } else{

        data <- rpostgis::pgGetRast(conn = con,
                                    name = sub_name,
                                    boundary = c(ext[4], ext[2],
                                                 ext[3], ext[1]))

      }
    }

    if(stringr::str_detect(name, "gpkg")){
      if(is.null(extent)){
      data <- sf::st_read(con, sub_name)
      } else{

        data <- sf::st_read(dsn = con,
                            query = glue::glue("SELECT * FROM ",
                                                       sub_name,
                                                       " WHERE ST_Intersects(geometry, ST_MakeEnvelope(",
                                                       as.numeric(ext[1]),", ",
                                                       as.numeric(ext[2]),", ",
                                                       as.numeric(ext[3]),", ",
                                                       as.numeric(ext[4]),", ",
                                                       as.numeric(meta$epsg),"))"),
                                               )



      }

    }


  } else {

    sub_name <- DBI::dbGetQuery(con,
                                glue::glue("SELECT sub_name FROM envdata.metadata WHERE folder_name = '",
                                           name,
                                           "'"))[,1]

    if(stringr::str_detect(name, "tif")){
      if(is.null(extent)){
      data <- rpostgis::pgGetRast(conn = con, name = sub_name)
      } else{
        data <- rpostgis::pgGetRast(conn = con,
                                    name = sub_name,
                                    boundary = c(ext[4], ext[2],
                                                 ext[3], ext[1]))
      }
    }

    if(stringr::str_detect(name, "gpkg")){

      if(is.null(extent)){
        data <- sf::st_read(con, sub_name)
      } else{

        data <- sf::st_read(dsn = con,
                            query = glue::glue("SELECT * FROM ",
                                               sub_name,
                                               " WHERE ST_Intersects(geometry, ST_MakeEnvelope(",
                                               as.numeric(ext[1]),", ",
                                               as.numeric(ext[2]),", ",
                                               as.numeric(ext[3]),", ",
                                               as.numeric(ext[4]),", ",
                                               as.numeric(meta$epsg),"))"),
        )



      }
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


    meta$date_of_download_db <- Sys.Date()

    if(!is.null(extent)){
      meta$extent_used <- "yes"
    }

    write.csv(meta,
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




