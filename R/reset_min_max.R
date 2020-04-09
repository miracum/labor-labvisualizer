#' @title reset_min_max
#'
#' @param db_data_subset The subset dataset
#' 
#' @inheritParams update_sliders 
#'
# reset_min_max
reset_min_max <- function(db_got_num, db_data_subset){
  
  # always update age slider
  min_age <- round(min(db_data_subset[, get("AGE")], na.rm = T), 2)
  max_age <- round(max(db_data_subset[, get("AGE")], na.rm = T), 2)
  
  # check, if we got numerical data
  if (isTRUE(db_got_num)){
    min <- round(min(db_data_subset[, get("VALUE_NUM")], na.rm = T), 2)
    max <- round(max(db_data_subset[, get("VALUE_NUM")], na.rm = T), 2)
  }
  return(list(
    min_age = min_age,
    max_age = max_age,
    min = min,
    max = max
  ))
}
