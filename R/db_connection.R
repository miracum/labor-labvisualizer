#' @title db_connection function
#' 
#' @inheritParams launch_app
#' 
#' @export
db_connection <- function(db_name, lib_path) {
  
  if (db_name == "ORACLE") {
    ## create driver
    drv <- RJDBC::JDBC(
      "oracle.jdbc.OracleDriver",
      classPath = paste0(
        lib_path, "/ojdbc7.jar"
      )
    )
    
    host <- Sys.getenv(
      paste0(db_name, "_HOST")
    )
    port <- Sys.getenv(
      paste0(db_name, "_PORT")
    )
    sid <- Sys.getenv(
      paste0(db_name, "_SID")
    )
    user <- Sys.getenv(
      paste0(db_name, "_USER")
    )
    password <- Sys.getenv(
      paste0(db_name, "_PASSWORD")
    )
    
    ## create URL
    url <- paste0("jdbc:oracle:thin:@//", host, ":", port, "/", sid)
  }
  
  ## create connection
  db_con <- DBI::dbConnect(
    drv = drv,
    url = url,
    user = user,
    password = password
  )

  return(db_con)
}