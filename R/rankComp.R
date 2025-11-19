#' Ranking Comparison
#'
#' Perform a ranking comparison between two indices.
#'
#' @param ref a numeric vector of reference index values.
#' @param alt a numeric vector of alternative index values.
#' @param highest.first a logical value indicating whether the highest value
#'  gets ranking #1. If `FALSE`, the lowest value gets ranking #1.
#' @param id a vector of unit identifiers.
#' @param time a vector of temporal factors.
#'
#' @returns
#' An object of classes `"rankComp"` and `"data.frame"` containing the following
#' columns:
#'  \item{id}{the unit identifiers (if provided).}
#'  \item{time}{the temporal factors (if provided).}
#'  \item{ref.rank}{the ranking based on the reference index.}
#'  \item{alt.rank}{the ranking based on the alternative index.}
#'  \item{shift}{the ranking shifts between two indices.}
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @seealso [`summary.rankComp`], [`rankScatterPlot`], [`rankShiftPlot`],
#'  [`rankRankPlot`].
#'
#' @example ./inst/examples/rankComp_eg.R
#'
#' @export
rankComp <- function(ref, alt, highest.first = TRUE, id = NULL, time = NULL) {
  if (!is.numeric(ref) || !is.numeric(alt)) {
    stop("Score vectors must be numeric.")
  }
  if (anyNA(ref) || anyNA(alt)) {
    stop("Missing value(s) found in score vectors!")
  }
  n <- length(ref)
  if (length(alt) != n) {
    stop("Length of score vectors must be equal.")
  }
  if (!is.null(id) && length(id) != n) {
    stop("Invalid length of id. vector!")
  }
  if (!is.null(time) && length(time) != n) {
    stop("Invalid length of temporal factor vector!")
  }
  if (!is.null(time) && anyNA(time)) {
    stop("Missing value(s) found in temporal factor vector!")
  }
  if (!is.null(time) && length(unique(time)) == 1) {
    warning("Vector of temporal factors is constant. Applied cross-sectional analysis.")
  }
  if (is.null(time)) time <- as.factor(rep(0, n))
  if (is.null(id)) id <- as.factor(rep(0, n))
  ref.rank <- list()
  alt.rank <- list()
  time.lst <- list()
  id.lst <- list()
  T <- sort(unique(time))
  for (t in T) {
    ref.rank[[t]] <- rankCal(x = ref[time == t], h = highest.first)
    alt.rank[[t]] <- rankCal(x = alt[time == t], h = highest.first)
    time.lst[[t]] <- time[time == t]
    id.lst[[t]] <- id[time == t]
  }
  ref.rank <- unlist(ref.rank)
  alt.rank <- unlist(alt.rank)
  time.lst <- unlist(time.lst)
  id.lst <- unlist(id.lst)
  comp <- data.frame(
    id = id.lst, time = time.lst, ref.rank = ref.rank,
    alt.rank = alt.rank, shift = ref.rank - alt.rank
  )
  if (length(T) == 1) comp$time <- NULL
  if (length(unique(time)) == 1) comp$id <- NULL
  rownames(comp) <- NULL
  class(comp) <- c("rankComp", "data.frame")
  return(comp)
}
