#' connect to database
#' So not change the credentials. This will automatically filled by the db_source file.
#'
#' @param intern are you in the izw network?
#' @export
#' @examples
#' \dontrun{
#' con_geodb()
#' }


con_geodb <- function(intern = TRUE){
  if(intern == TRUE){
  con <- DBI::dbConnect(RPostgres::Postgres(),
                   dbname = db,
                   host=host_db,
                   port=db_port,
                   user=db_user,
                   password=db_password)
  }
  if(intern == FALSE){
    con <- DBI::dbConnect(RPostgres::Postgres(),
              dbname = db,
              host=host_db_ex,
              port=db_port_ex,
              user=db_user,
              password=db_password)
  }
  return(con)

}
