#' @title summary_function function
#' 
#' @param vector A vector containing continuous values.
#' 
#' @export
#' 
summary_function <- function(vector){
  ret <- data.table(rbind(
    c("Min:", round(min(vector, na.rm = T), 2)),
    c("Mean:", round(mean(vector, na.rm = T), 2)),
    c("Median:", round(stats::median(vector, na.rm = T), 2)),
    c("Max:", round(max(vector, na.rm = T), 2)),
    c("SD:", round(stats::sd(vector, na.rm = T), 2)),
    c("Skewness:", round(e1071::skewness(vector, na.rm = T), 2)),
    c("Missing:", sum(is.na(vector)))
  ))
  colnames(ret) <- c("", "")
  return(ret)
}
