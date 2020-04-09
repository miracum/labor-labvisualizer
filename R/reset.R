#' @title reset
#'
#' @param db_got_cat A logical, indicates if only discrete data is present.
#' @param db_data The dataset
#' 
#' @inheritParams module_visualize_server 
#' @inheritParams reset_min_max 
#'
# reset
reset <- function(session, db_got_num, db_got_cat, db_data) {
  
  # check, if we got numerical data
  if (isTRUE(db_got_num)){
    db_data_subset <- db_data[!is.na(get("VALUE_NUM")), ]
  } else if (isTRUE(db_got_cat)){
    db_data_subset <- db_data
  }
  
  # reset gender & outliers
  gender_present <- "ALL"
  
  if(isTRUE(db_got_num)){
    if (abs(e1071::skewness(db_data[, get("VALUE_NUM")], na.rm = T)) > 30){
      outlier_default <- T
    } else {
      outlier_default <- F
    }
  }
  
  # reset min/max
  min_max_list <- reset_min_max(
    db_got_num = db_got_num,
    db_data_subset = db_data_subset
  )
  
  # reset sliders
  update_slider(
    session = session,
    db_got_num = db_got_num,
    range_min = min_max_list$min,
    range_max = min_max_list$max,
    min_age = min_max_list$min_age,
    max_age = min_max_list$max_age
  )
  
  return(
    list(
      db_data_subset = db_data_subset,
      gender_present = gender_present,
      min_max_list = min_max_list,
      outlier_default
    )
  )
}
