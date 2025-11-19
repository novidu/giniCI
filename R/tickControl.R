#' Tick Control
#' @noRd
tickControl <- function(id, max.tick = 50) {
  if (length(id) <= max.tick) {
    return(id)
  } else {
    incre <- ceiling(length(id) / max.tick)
    return(id[seq_along(id) %% incre == 1])
  }
}
