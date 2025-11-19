#' Quantile Ranking Calculation
#' @noRd
nRank <- function(x, n) {
  as.numeric(cut(-x, breaks = quantile(-x,
    probs = seq(0, 1, length = n + 1),
    na.rm = TRUE
  ), include.lowest = TRUE))
}
