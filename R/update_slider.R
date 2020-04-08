#' @title update_sliders
#'
#' @param db_got_num A logical, indicates if VALUE_NUM is present and the 
#'   range slider needs to be populated.
#' @param range_min The minimum value of VALUE NUM (default: NULL)
#' @param range_max The maximum value of VALUE NUM (default: NULL)
#' @param min_age The minimum age value of the underlying cohort.
#' @param max_age The maximum age value of the underlying cohort.
#' 
#' @inheritParams module_visualize_server 
#'
#' @export
#'
# update_sliders
update_sliders <- function(
  session,
  db_got_num,
  range_min = NULL,
  range_max = NULL,
  min_age,
  max_age) {
  # always update age slider
  # update ageSlider to min/max
  updateSliderInput(session, "age_slider", value = c(min_age, max_age))
  
  # check, if we got numerical data
  if (db_got_num){
    # update rangeSlider to min/max
    updateSliderInput(session, "range_slider", value = c(min, max))
  }
}
