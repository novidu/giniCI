#' Ranking Calculation
#' @noRd
rankCal <- function(x, h = TRUE) {
  if (h == TRUE) {
    return(as.numeric(factor(-rank(x, na.last = "keep"))))
  } else {
    return(as.numeric(factor(rank(x, na.last = "keep"))))
  }
}
