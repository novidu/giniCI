#' Summarizing Ranking Comparison
#'
#' Summary method for class `"rankComp"` and print method for class
#' `"summary.rankComp"`.
#'
#' @name summary.rankComp
#' @rdname summary.rankComp
#'
#' @param object an object of class `"rankComp"`, usually, an output of a call
#'  to [`rankComp`].
#' @param n.pick a positive integer specifying the number of units considered to
#'  form the top/bottom based on the alternative index. The default value is `10`.
#' @param n.q a positive integer specifying the n-quantiles considered
#'  to compute quantile rankings. The default value is `10` (deciles).
#' @param x an object of class `"summary.rankComp"`, usually, a output of a call
#'  to [`summary.rankComp`].
#' @param digits number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' `summary.rankComp` provides details on the ranking comparison between the
#' reference and the alternative indices stored in the object of class
#' `"rankComp"`. `print.summary.rankComp` prints summary information using a
#' smart digit format for the components.
#'
#' @returns
#' An object of class `"summary.rankComp"` which is a list of components:
#'  \item{par}{a vector storing the values of `n.pick` and `n.q`.}
#'  \item{n.unit}{the number of ranked units (by temporal factors if available).}
#'  \item{shift.stats}{a data frame with rows presenting the summary statistics
#'   of ranking shifts: minimum, first quartile, median, mean, third quartile,
#'   and maximum.}
#'  \item{asr}{a data frame giving the average shift in ranking (ASR)
#'   \deqn{\mathrm{ASR} = \frac{1}{m} \sum^m_{i = 1}
#'   | \mathrm{rank}^{\mathrm{alt}}_i - \mathrm{rank}^{\mathrm{ref}}_i |,}
#'   where \eqn{m} is the number of units considered. The rows present the ASR
#'   for all units, and for the top and the bottom units based on the
#'   alternative index ranking.}
#'  \item{per}{a data frame giving the percentage of equal rankings (PER)
#'   \deqn{\mathrm{PER} = 100 \times \frac{1}{m} \sum^m_{i = 1}
#'   \{ \mathrm{rank}^{\mathrm{alt}}_i = \mathrm{rank}^{\mathrm{ref}}_i \},}
#'   where \eqn{m} is the number of units considered. The rows present the PER
#'   for all units, and for the top and the bottom units based on the
#'   alternative index ranking.}
#'  \item{asq}{the average shift in quantile ranking (by temporal factors if
#'   available). This value is similar to the ASR for all units, but using the
#'   quantile ranking of two indices.}
#' For `shift.stats`, `asr`, and `per`, multiple columns will be generated
#' according to temporal factors if `object$time` is not `NULL`.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @references
#' Mariani, F., Ciommi, M., & Recchioni, M. C. (2024). Two in One: A New Tool
#' to Combine Two Rankings Based on the Voronoi Diagram. \emph{Social Indicators
#' Research}, 175, 989--1005.
#'
#' @seealso [`rankComp`].
#'
#' @example ./inst/examples/rankComp_eg.R
#'
NULL
#'
#' @rdname summary.rankComp
#' @export
summary.rankComp <- function(object, n.pick = 10L, n.q = 10L, ...) {
  if (n.pick %% 1 != 0) {
    stop("n.pick must be an integer.")
  }
  if (n.q %% 1 != 0) {
    stop("n.q must be an integer.")
  }
  z <- object
  z.sum <- list()
  if (is.null(z$time)) z$time <- as.factor("(*)")
  for (t in unique(z$time)) {
    z.t <- z[z$time == t, ]
    n <- nrow(z.t)
    if (n.pick > n) {
      stop("n.pick cannot surpass the number of units.")
    }
    if (n.pick <= 0L) {
      stop("n.pick must be positive.")
    }
    n.unq.r <- max(z.t$alt.rank)
    if (n.q > n.unq.r) {
      stop("n.q cannot surpass the number of unique rankings in the alternative index.")
    }
    if (n.q <= 0L) {
      stop("n.q must be positive.")
    }
    z.t.top <- z.t[with(z.t, order(alt.rank)), ][1:n.pick, ]
    z.t.bot <- z.t[with(z.t, order(-alt.rank)), ][1:n.pick, ]
    z.t$ref.rank.iq <- nRank(z.t$ref.rank, n = n.q)
    z.t$alt.rank.iq <- nRank(z.t$alt.rank, n = n.q)
    asr <- sum(abs(z.t$shift)) / n
    asr.top <- sum(abs(z.t.top$shift)) / n.pick
    asr.bot <- sum(abs(z.t.bot$shift)) / n.pick
    per <- 100 * (sum(z.t$shift == 0)) / n
    per.top <- 100 * (sum(z.t.top$shift == 0)) / n.pick
    per.bot <- 100 * (sum(z.t.bot$shift == 0)) / n.pick
    asq <- sum(abs(z.t$alt.rank.iq - z.t$ref.rank.iq)) / n
    z.sum$n.unit[[t]] <- n
    z.sum$shift.stats[[t]] <- as.numeric(summary(z.t$shift))
    z.sum$asr[[t]] <- c(asr, asr.top, asr.bot)
    z.sum$per[[t]] <- c(per, per.top, per.bot)
    z.sum$asq[[t]] <- asq
  }
  z.sum$par <- c(n.pick = n.pick, n.q = n.q)
  z.sum$n.unit <- unlist(z.sum$n.unit)
  z.sum$shift.stats <- do.call(cbind.data.frame, z.sum$shift.stats)
  rownames(z.sum$shift.stats) <- c("Min.", "1st Q.", "Median", "Mean", "3rd Q.", "Max.")
  z.sum$asr <- do.call(cbind.data.frame, z.sum$asr)
  rownames(z.sum$asr) <- c("ASR", paste0("ASR.T", n.pick), paste0("ASR.B", n.pick))
  z.sum$per <- do.call(cbind.data.frame, z.sum$per)
  rownames(z.sum$per) <- c("PER", paste0("PER.T", n.pick), paste0("PER.B", n.pick))
  z.sum$asq <- do.call(cbind.data.frame, z.sum$asq)
  rownames(z.sum$asq) <- paste0("ASQ.", n.q)
  class(z.sum) <- "summary.rankComp"
  return(z.sum)
}

#' @rdname summary.rankComp
#' @export
print.summary.rankComp <- function(x, digits = max(3L, getOption("digits") - 3L),
                                   ...) {
  cat("Number of ranked units: \n")
  print(x$n.unit)
  cat("\n")
  shift <- x$shift.stats
  cat("Ranking shift summary statistics: \n")
  print(shift, digits = digits)
  cat("\n")
  asr <- x$asr
  rownames(asr) <- c(
    "All units",
    paste0("Top ", x$par[1]),
    paste0("Bottom ", x$par[1])
  )
  cat("Average shift in ranking: \n")
  print(asr, digits = digits)
  cat("\n")
  per <- x$per
  rownames(per) <- c(
    "All units",
    paste0("Top ", x$par[1]),
    paste0("Bottom ", x$par[1])
  )
  cat("Percentage of equal rankings: \n")
  print(per, digits = digits)
  cat("\n")
  cat(paste0("Average shift in ", x$par[2], "-quantile ranking:\n"))
  print(x$asq, row.names = FALSE, digits = digits)
  cat("\n")
  if (length(x$n.unit) == 1) {
    cat("---\n")
    cat("Note: (*) cross-sectional analysis")
  }
  invisible(x)
}
