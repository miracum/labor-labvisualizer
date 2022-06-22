#' @title cap_outliers function
#'
#' @inheritParams summary_function
#'
#' @export
#'
cap_outliers <- function(vector) {
  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  # get quantiles of vector
  q <- stats::quantile(
    vector,
    probs = c(.25, .75),
    na.rm = TRUE,
    names = FALSE
  )

  # IQR
  i <- stats::IQR(
    vector,
    na.rm = TRUE
  )

  # OutLo: < Q25-1.5*IQR
  out_lo <- q[1] - 1.5 * i

  # OutHi: > Q75+1.5*IQR
  out_hi <- q[2] + 1.5 * i

  # nolint start
  # # cap low outliers
  # vector[which(vector < OutLo)] <- OutLo
  #
  # # cap high outliers
  # vector[which(vector > OutHi)] <- OutHi
  # nolint end

  # remove low outliers
  vector[which(vector < out_lo)] <- NA

  # cap high outliers
  vector[which(vector > out_hi)] <- NA

  # return vector
  return(vector)
}
