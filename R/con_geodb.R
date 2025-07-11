#' connect to database
#' So not change the credentials. This will automatically filled by the db_source file.
#'
#' @param db name of database
#' @param host_db host name
#' @param db_port port
#' @param db_user user_name
#' @param host_db_ex external hostname
#' @param db_port_ex external port
#' @param db_password password
#' @param intern are you in the izw network?
#' @export
#' @examples
#' \dontrun{
#' con_geodb()
#' }


con_geodb <- function(db = db,
                      host_db = host_db,
                      db_port = db_port,
                      db_user = db_user,
                      db_password = db_password,
                      host_db_ex = host_db_ex,
                      db_port_ex = db_port_ex,
                      intern = TRUE){
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
