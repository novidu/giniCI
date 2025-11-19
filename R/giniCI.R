#' Computing Gini-based Composite Indicators
#'
#' Compute a composite index with weighting schemes based on the Gini
#' coefficient of constituent indicators, with options for aggregation methods
#' and  horizontal variability adjustment.
#'
#' @usage
#' giniCI(inds, method = c("equal", "gini", "reci"),
#'        agg = c("ari", "geo"), hv = TRUE, ci.pol = c("pos", "neg"),
#'        time = NULL, ref.time = NULL, only.ci = FALSE)
#'
#' @param inds a matrix or data frame of indicators to be aggregated.
#' @param method weighting method to be used. See `'Details'`.
#' @param agg aggregation function to be used, with `"ari"` denoting the
#'   weighted arithmetic mean and `"geo"` denoting the weighted geometric mean.
#' @param hv a logical value indicating whether horizontal variability adjustment
#'   should be applied.
#' @param ci.pol a character value indicating the polarity of composite index.
#'   Use `"pos"` if increasing values of the composite index correspond to
#'   positive variations of the phenomenon (e.g., socio-economic developments).
#'   Otherwise, use `"neg"` if increasing values of the composite index
#'   correspond to to negative variations of the phenomenon (e.g., vulnerability
#'   and poverty).
#' @param time a vector of temporal factors for indicators. The length of `time`
#'   must equal the number of rows in `inds`. If `NULL`, the input indicators
#'   are treated as cross-sectional data.
#' @param ref.time a value denoting the reference time for weighting. If
#'   provided, weights will be derived using only observations at the reference
#'   time.
#' @param only.ci a logical value indicating whether only the composite index
#'   should be returned.
#'
#' @details
#' The defaut method is `"equal"` that produces equal weights \eqn{1/n} where
#' \eqn{n} is the number of indicators. For methods `"gini"` (Gini-based
#' weighting) and `"reci"` (reciprocal Gini-based weighting), weights are
#' defined based on the Gini coefficient of indicators. Let \eqn{G_i} be the
#' Gini coefficient of the \eqn{i}-th indicator, the weights by methods
#' `"gini"` and `"reci"` are respectively computed as
#' \eqn{w^\mathrm{gini}_i = \frac{G_i}{\sum^n_{i = 1} G_i}} and
#' \eqn{w^\mathrm{reci}_i = \frac{1 / G_i}{\sum^n_{i = 1} 1 / G_i}}.
#'
#' Temporal factors can be applied to methods `"gini"` and `"reci"`. If either
#' `time` or `ref.time` is `NULL`, the weighting process is run on all
#' observations. If both `time` and `ref.time` are not `NULL`, only observations
#' at the reference time are used for weight computation.
#'
#' When aggregating the indicators, the aggregate score for the \eqn{j}-th unit
#' is computed by applying the chosen aggregation function with the obtained
#' weights to values in the \eqn{j}-th row. If `hv = TRUE`, horizontal
#' variability adjustment is executed by introducing a penalty for units with
#' unbalanced values among dimensions. The penalty for the \eqn{j}-th unit is
#' defined as the the index of dispersion (variance-to-mean ratio) of values in
#' the \eqn{j}-th row. If `ci.pol = "pos"`, the penalties is subtracted from the
#' aggregate scores to form the composite index. If `ci.pol = "neg"` the
#' penalties is added to the aggregate scores to form the composite index.
#'
#' @returns
#' A list containing the following components:
#'  \item{ci}{the composite index.}
#'  \item{w}{the weights assigned.}
#'  \item{pen}{the horizontal variability penalties (if `hv = TRUE`).}
#' If `only.ci = TRUE`, the function will return only the composite index.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @references
#' Gini, C. (1914). Sulla misura della concentrazione e della variabilita dei
#' caratteri. \emph{Atti del Reale Istituto Veneto di Scienze, Lettere ed Arti},
#' 62(5), 1203--1248.
#'
#' Mazziotta, M., & Pareto, A. (2016). On a Generalized Non-compensatory
#' Composite Index for Measuring Socio-economic Phenomena. \emph{Social
#' Indicators Research}, 127, 983--1003.
#'
#' Ciommi, M., Gigliarano, C., Emili, A., Taralli, S., & Chelli, F. M. (2017).
#' A new class of composite indicators for measuring well-being at the local
#' level: An application to the Equitable and Sustainable Well-being (BES) of
#' the Italian Provinces. \emph{Ecological Indicators}, 76, 281--296.
#'
#' @note
#' Methods `"gini"` and `"reci"` require non-negative indicators for the
#' calculation of Gini coefficients. In addition, option `hv = TRUE` cannot be
#' used if any row contains negative values. Therefore, it may be necessary to
#' use [`normalize`] to scale the indicators to non-negative ranges before
#' computing the composite index.
#'
#' A Gini coefficient of zero occurs when the indicators are constant or do not
#' change over the reference time. If a zero Gini coefficient is obtained for
#' for one or more indicators, method `"gini"` returns the corresponding weights
#' as zero while method `"reci"` cannot be applied.
#'
#' @seealso
#' [`normalize`], [`rankComp`].
#'
#' @example ./inst/examples/giniCI_eg.R
#'
#' @export
giniCI <- function(inds, method = c("equal", "gini", "reci"),
                   agg = c("ari", "geo"), hv = TRUE, ci.pol = c("pos", "neg"),
                   time = NULL, ref.time = NULL, only.ci = FALSE) {
  inds <- as.data.frame(inds)
  n <- ncol(inds)
  method <- match.arg(method)
  agg <- match.arg(agg)
  ci.pol <- match.arg(ci.pol)
  if (n == 1) {
    stop("The number of indicators must be at least two.")
  }
  if (all(sapply(inds, is.numeric)) == FALSE) {
    stop("All indicators must be numeric.")
  }
  if (!is.null(time) & length(time) != nrow(inds)) {
    stop("Invalid length of temporal factor vector!")
  }
  if (!is.null(ref.time) & is.null(time)) {
    stop("Vector of temporal factors is missing.")
  }
  if (!is.null(ref.time)) {
    if (!ref.time %in% unique(time)) stop("Invalid reference time!")
  }
  if (!is.null(time) & is.null(ref.time)) {
    warning("Reference time is missing. Input dataset is treated as cross-sectional.",
      call. = FALSE
    )
  }
  if (agg == "ari") {
    f <- function(df, w) {
      return(apply(t(t(df) * w), 1, sum))
    }
  } else {
    f <- function(df, w) {
      return(apply(t(t(df)^w), 1, prod))
    }
  }
  if (sum(inds < 0, na.rm = TRUE) > 0) {
    n.cell.neg <- sum(inds < 0, na.rm = TRUE)
    col.neg <- apply(inds, 2, function(x) all(x >= 0, na.rm = TRUE))
    col.neg <- which(col.neg == FALSE)
    if (method != "equal") {
      stop(paste0(
        n.cell.neg, " negative value(s) found in columns(s) ",
        noquote(paste(col.neg, collapse = ", ")),
        ". Gini-based weighting requires non-negative indicators. ",
        "Consider normalizing indicators before using giniCI."
      ))
    } else {
      if (hv == TRUE) {
        stop(paste0(
          n.cell.neg, " negative value(s) found in columns(s) ",
          noquote(paste(col.neg, collapse = ", ")),
          ". Cannot apply horizontal variability adjustment. ",
          "Consider normalizing indicators before using giniCI."
        ))
      }
      if (agg == "geo") {
        stop(
          paste0(
            n.cell.neg, " negative value(s) found in columns(s) ",
            noquote(paste(col.neg, collapse = ", ")),
            ". Geometric aggregation requires non-negative indicators. ",
            "Consider normalizing indicators before using giniCI."
          ),
          call. = FALSE
        )
      }
    }
  }
  if (hv == TRUE) {
    V <- apply(inds, 1, function(x) var(x) * (n - 1) / n)
    M <- rowMeans(inds)
    if (any(M == 0, na.rm = TRUE)) {
      stop("Zero row mean(s) detected. Cannot apply horizontal variability adjustment.")
    }
    pen <- V / M
  } else {
    pen <- rep(0, nrow(inds))
  }
  if (method == "equal") {
    w <- rep(1 / n, n)
    names(w) <- colnames(inds)
  } else {
    if (is.null(time) || is.null(ref.time)) {
      gi <- apply(inds, 2, function(x) DescTools::Gini(x, na.rm = TRUE))
    } else {
      gi <- apply(
        inds[time == ref.time, ], 2,
        function(x) DescTools::Gini(x, na.rm = TRUE)
      )
    }
    if (sum(gi == 0) > 0) {
      n.gini.zero <- sum(gi == 0)
      gini.zero <- which(gi == 0)
      warning(
        paste0(
          n.gini.zero, " Gini coefficient(s) of zero computed for columns(s) ",
          noquote(paste(gini.zero, collapse = ", ")), "."
        ),
        call. = FALSE
      )
      if (sum(gi) == 0) {
        stop("All Gini coefficients are zero. Weights cannot be computed.")
      }
      if (method == "gini") {
        warning(n.gini.zero, " weight(s) returned as zero.", call. = FALSE)
      } else {
        stop("Reciprocal Gini-based weighting cannot be applied.")
      }
    }
    if (method == "gini") w <- gi / sum(gi) else w <- (1 / gi) / sum(1 / gi)
  }
  if (ci.pol == "neg") ci <- f(inds, w) + pen else ci <- f(inds, w) - pen
  if (sum(is.nan(ci)) > 0) {
    n.nan <- sum(is.nan(ci))
    warning(paste(n.nan, "NaN value(s) generated in the composite index."),
      call. = FALSE
    )
  }
  if (sum(is.na(ci)) != sum(is.nan(ci))) {
    n.missing <- sum(is.na(ci)) - sum(is.nan(ci))
    warning(paste(n.missing, "NA value(s) generated in the composite index."),
      call. = FALSE
    )
  }
  if (isTRUE(only.ci)) {
    return(ci)
  } else {
    if (isFALSE(hv)) {
      return(list(ci = ci, w = w))
    } else {
      return(list(ci = ci, w = w, pen = pen))
    }
  }
}
