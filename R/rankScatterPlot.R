#' Rank Scatter Plot
#'
#' Generate rank scatter plots for ranking comparison.
#'
#' @usage
#' rankScatterPlot(object, p.col = "black", p.size = 1.5, p.shape = 19,
#'                 lab = TRUE, lab.col = "red", lab.size = 3.88,
#'                 ref.line = TRUE, max.overlaps = 10, max.tick = 50,
#'                 ref.lab = "Reference ranking", alt.lab = "Alternative ranking",
#'                 combine = FALSE, nr = NULL, nc = NULL)
#'
#' @param object an object of class `"rankComp"`, usually, an output of a call
#'   to [`rankComp`].
#' @param p.col point color code. See \sQuote{Color Specification}
#'   in [`par`].
#' @param p.size point size value.
#' @param p.shape point shape value. See \sQuote{pch values} in [`points`].
#' @param lab a logical value indicating whether identifier labels should be
#'   assigned to scatter points. If `object$id` is `NULL`, the points
#'   will be labeled by the row index.
#' @param lab.col color code for labels. See \sQuote{Color Specification} in
#'   [`par`].
#' @param lab.size label size value.
#' @param ref.line a logical value indicating whether a 45-degree reference line
#'   should be added to the plot.
#' @param max.overlaps a value to exclude the label if it has too many overlaps.
#'   The default value is `10`. Set `max.overlaps = Inf` to always show all
#'   labels.
#' @param max.tick a positive integer to control the maximum number of axis
#'   ticks. The default value is `50`. Set `max.tick` equal to or greater than
#'   the number of rankings to display all rankings on the axis.
#' @param ref.lab name of the reference index.
#' @param alt.lab name of the alternative index.
#' @param combine a logical value indicating whether to generate a grid that
#'   combines plots from different time factors (If `object$time` is not `NULL`).
#' @param nr (optional) number of rows in the plot grid.
#' @param nc (optional) number of columns in the plot grid.
#'
#' @returns
#' A scatter plot displaying unit rankings for two indices. In case `object$time`
#' is not `NULL`, a list of plots for different time factors and the combined
#' grid (if `combine = TRUE`) will be returned. The function does not print the
#' return if it is assigned to an object. Use [`print`] with the storing object
#' to generate the plot.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @seealso [`rankComp`], [`rankShiftPlot`], [`rankRankPlot`].
#'
#' @example ./inst/examples/plot_eg.R
#'
#' @export
rankScatterPlot <- function(object, p.col = "black", p.size = 1.5, p.shape = 19,
                            lab = TRUE, lab.col = "red", lab.size = 3.88,
                            ref.line = TRUE, max.overlaps = 10, max.tick = 50,
                            ref.lab = "Reference ranking",
                            alt.lab = "Alternative ranking",
                            combine = FALSE, nr = NULL, nc = NULL) {
  if (is.null(object$id)) object$id <- 1:nrow(object)
  if (is.null(object$time)) object$time <- as.factor(rep(0, nrow(object)))
  p <- list()
  for (t in sort(unique(object$time))) {
    loc <- object$time == t
    p[[t]] <- ggplot(
      data = object[loc, ],
      aes(x = ref.rank, y = alt.rank, label = id)
    ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(
        breaks = tickControl(1:max(object[loc, ]$ref.rank),
          max.tick = max.tick
        )
      ) +
      scale_y_continuous(
        breaks = tickControl(1:max(object[loc, ]$alt.rank),
          max.tick = max.tick
        )
      ) +
      labs(title = t, x = ref.lab, y = alt.lab)
    if (isTRUE(ref.line)) {
      p[[t]] <- p[[t]] + geom_abline(intercept = 0, slope = 1)
    }
    p[[t]] <- p[[t]] + geom_point(color = p.col, shape = p.shape, size = p.size)
    if (isTRUE(lab)) {
      p[[t]] <- p[[t]] + ggrepel::geom_text_repel(
        max.overlaps = max.overlaps,
        size = lab.size,
        color = lab.col
      )
    }
  }
  if (length(p) == 1) {
    p <- p[[1]]
    p <- p + labs(title = NULL)
    return(p)
  } else {
    if (isTRUE(combine)) {
      p.comb <- ggpubr::ggarrange(plotlist = p, nrow = nr, ncol = nc)
      p[["comb"]] <- p.comb
    }
    return(p)
  }
}
