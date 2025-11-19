#' Indicator Normalization
#'
#' Perform normalization based on indicators' polarity.
#'
#' @usage
#' normalize(inds, method = c("min-max", "goalpost"), ind.pol,
#'           gp.range = c(70, 130), time = NULL, ref.time = NULL,
#'           ref.value = NULL)
#'
#' @param inds a numeric vector, matrix, or data frame which provides indicators
#'   to be normalized.
#' @param method normalization method to be used. See \sQuote{Details}.
#' @param ind.pol a character vector whose elements can be `"pos"` (positive) or
#'   `"neg"` (negative), indicating the polarity of indicators. An indicator's
#'   polarity is the sign of the relation between the indicator and the
#'   phenomenon to be measured.
#' @param gp.range a vector of the form `c(a,b)` giving the normalization range
#'   for method `"goalpost"`. The default value is `c(70,130)`.
#' @param time a vector of temporal factors for input indicators. The length of
#'   `time` must equal the number of rows in `inds`. If `NULL`, the input data
#'   will be treated as cross-sectional.
#' @param ref.time a value denoting the reference time for normalization. See
#'   \sQuote{Details}.
#' @param ref.value a vector containing reference values for indicators to
#'   facilitate the interpretation of results, required by method `"goalpost"`.
#'   When normalizing each indicator, their reference values will be mapped to
#'   the midpoint of `gp.range`. See \sQuote{Details}.
#'
#' @details
#' By default, each indicator \eqn{x} is normalized by method `"min-max"` with
#' the formulas
#' \deqn{\displaystyle\tilde{x}^{+}_i = \frac{x_i -
#' \mathrm{inf}_x}{\mathrm{sup}_x - \mathrm{inf}_x},}
#' or
#' \deqn{\displaystyle \tilde{x}^{-}_i = 1 - \tilde{x}^{+}_i,}
#' where \eqn{\mathrm{sup}_x} and \eqn{\mathrm{inf}_x} are respectively the
#' superior and inferior values of the indicator. The former formula is applied
#' to indicators with positive polarity while the latter one is used for those
#' with negative polarity.
#'
#' If either `time` or `ref.time` is `NULL`, the superior and inferior values
#' are respectively the maximum and minimum values of \eqn{x}. If both `time`
#' and `ref.time` are not `NULL`, the superior and inferior values are
#' respectively the maximum and minimum values of \eqn{x} observed at the
#' reference time. In other words, if `time` is not provided or provided without
#' specifying a value for `ref.time`, the input data will be treated as
#' cross-sectional.
#'
#' For method `"goalpost"`, a vector of reference values for indicators is
#' required. If not specified by users (`ref.value = NULL`), these values are
#' automatically set #' to the indicator means for cross-sectional data or to
#' the indicator means at the reference time for longitudinal data.
#'
#' Method `"goalpost"` computes two goalposts for normalization as
#' \eqn{\mathrm{gp\_min}_x = \mathrm{ref}_x - \Delta} and
#' \eqn{\mathrm{gp\_max}_x = \mathrm{ref}_x + \Delta}, where
#' \eqn{\mathrm{ref}_x} is the reference value of \eqn{x} and
#' \eqn{\Delta = (\mathrm{sup}_x - \mathrm{inf}_x)/2}. Indicators with
#' positive polarity are rescaled using the formula
#' \deqn{\displaystyle \tilde{x}^{+}_i =
#' \frac{x_i - \mathrm{gp\_min}_x}{\mathrm{gp\_max}_x -
#' \mathrm{gp\_min}_x} (b - a) + a,}
#' while indicators with negative polarity are rescaled using the formula
#' \deqn{\displaystyle \tilde{x}^{-}_i = a + b - \tilde{x}^{+}_i.}
#' If an indicator follows a symmetric probability distribution and its
#' reference value is set to the mean, the normalized values will theoretically
#' remain in the range \eqn{[a,b]}. In other cases, the normalized values may
#' extend beyond `gp.range`.
#'
#' @returns An object of class `"data.frame"` containing normalized indicators.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @references
#' Mazziotta, M., & Pareto, A. (2016). On a Generalized Non-compensatory
#' Composite Index for Measuring Socio-economic Phenomena. \emph{Social
#' Indicators Research}, 127, 983--1003.
#'
#' @seealso
#' [`giniCI`].
#'
#' @example ./inst/examples/normalize_eg.R
#'
#' @export
normalize <- function(inds, method = c("min-max", "goalpost"), ind.pol,
                      gp.range = c(70, 130), time = NULL, ref.time = NULL,
                      ref.value = NULL) {
  name <- deparse(substitute(inds))
  inds <- as.data.frame(inds)
  n <- ncol(inds)
  method <- match.arg(method)
  if (n == 1) colnames(inds) <- name
  if (all(sapply(inds, is.numeric)) == FALSE) {
    stop("All indicators must be numeric.")
  }
  if (length(ind.pol) != n) {
    stop("Length of polarity vector must equal the number of indicators.")
  }
  if (all(unique(ind.pol) %in% c("pos", "neg")) == FALSE) {
    stop("Values of polarity vector must be either 'pos' or 'neg'.")
  }
  if (gp.range[1] >= gp.range[2]) {
    stop("Invalid range, the first value must be smaller than the second value.")
  }
  if (any(gp.range < 0)) {
    stop("Range must contain only non-negative values.")
  }
  if (!is.null(time) && length(time) != nrow(inds)) {
    stop("Invalid length of temporal factor vector!")
  }
  if (!is.null(ref.time) && is.null(time)) {
    stop("Vector of temporal factors is missing.")
  }
  if (!is.null(ref.time)) {
    if (!ref.time %in% unique(time)) stop("Invalid reference time!")
  }
  if (!is.null(time) && is.null(ref.time)) {
    warning("Reference time is missing. Input dataset is treated as cross-sectional.",
      call. = FALSE
    )
  }
  inds.norm <- list()
  if (is.null(time) || is.null(ref.time)) {
    inf <- apply(inds, 2, function(x) min(x, na.rm = TRUE))
    sup <- apply(inds, 2, function(x) max(x, na.rm = TRUE))
  } else {
    inf <- apply(inds[time == ref.time, ], 2, function(x) min(x, na.rm = TRUE))
    sup <- apply(inds[time == ref.time, ], 2, function(x) max(x, na.rm = TRUE))
  }
  for (i in 1:n) {
    if (sup[i] == inf[i]) {
      stop(paste("Column", i, "is constant (or constant at the reference time). Cannot perform normalization!"))
    }
  }
  if (method == "min-max") {
    for (i in 1:n) {
      ind <- (inds[, i] - inf[i]) / (sup[i] - inf[i])
      if (ind.pol[i] == "neg") ind <- 1 - ind
      inds.norm[[i]] <- ind
    }
  } else {
    if (is.null(ref.value)) {
      if (is.null(ref.time)) {
        ref.value <- colMeans(inds, na.rm = TRUE)
      } else {
        ref.value <- colMeans(inds[time == ref.time, ], na.rm = TRUE)
      }
    } else {
      if (length(ref.value) != n) {
        stop("Length of reference value vector must equal the number of indicators.")
      }
    }
    delta <- (sup - inf) / 2
    gp.min <- ref.value - delta
    gp.max <- ref.value + delta
    for (i in 1:n) {
      ind <- (inds[, i] - gp.min[i]) / (gp.max[i] - gp.min[i]) *
        (gp.range[2] - gp.range[1]) + gp.range[1]
      if (ind.pol[i] == "neg") ind <- gp.range[1] + gp.range[2] - ind
      inds.norm[[i]] <- ind
    }
  }
  inds.norm <- do.call(cbind.data.frame, inds.norm)
  colnames(inds.norm) <- colnames(inds)
  rownames(inds.norm) <- rownames(inds)
  if (!all(complete.cases(inds.norm))) {
    n.missing <- sum(is.na(inds.norm))
    warning(paste(n.missing, "NA value(s) generated."),
      call. = FALSE
    )
  }
  if (sum(inds.norm < 0, na.rm = TRUE) > 0) {
    n.cell.neg <- sum(inds.norm < 0)
    col.neg <- apply(inds.norm, 2, function(x) all(x >= 0, na.rm = TRUE))
    col.neg <- which(col.neg == FALSE)
    warning(
      paste(
        n.cell.neg, "negative value(s) generated in columns(s):",
        noquote(paste(col.neg, collapse = ", "))
      ),
      call. = FALSE
    )
  }
  return(inds.norm)
}
