#' @title load_sql function
#' 
#' @inheritParams launch_app
#' 
#' @export
#'
load_sql <- function(db_name, utils_path) {
  # load json with sql-queries
  sql <- jsonlite::fromJSON(
    txt = paste0(utils_path, "/SQL/", dbname, "_sql.JSON")
  )
  return(sql)
}