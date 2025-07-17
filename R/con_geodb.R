#' connect to database
#' Provides a robust and flexible database connection utility
#' that automatically manages the geodata-base connection between internal and external
#' database hosts.
#'
#' @export
#' @examples
#' \dontrun{
#' con_geodb()
#' }


con_geodb <- function() {
  # Versuch der internen Verbindung
  tryCatch({
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = db,
                          host=host_db,
                          port=db_port,
                          user=db_user,
                          password=db_password)

    # Teste die Verbindung mit einem einfachen Ping
    test_query <- DBI::dbGetQuery(con, "SELECT 1")

    message("Internal connection successful")
    return(con)
  },
  error = function(e) {
    message("Internal connection failed. Attempting external connection...")

    # Externe Verbindung
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = db,
                          host=host_db_ex,
                          port=db_port_ex,
                          user=db_user,
                          password=db_password)

    message("External connection successful")
    return(con)
  })
}


# con_geodb <- function(intern = TRUE){
#   if(intern == TRUE){
#     con <- DBI::dbConnect(RPostgres::Postgres(),
#                           dbname = db,
#                           host=host_db,
#                           port=db_port,
#                           user=db_user,
#                           password=db_password)
#   }
#   if(intern == FALSE){
#     con <- DBI::dbConnect(RPostgres::Postgres(),
#                           dbname = db,
#                           host=host_db_ex,
#                           port=db_port_ex,
#                           user=db_user,
#                           password=db_password)
#   }
#   return(con)
#
# }


