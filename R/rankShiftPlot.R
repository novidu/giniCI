#' Rank Shift Plot
#'
#' Generate rank shift plots for ranking comparison.
#'
#' @usage
#' rankShiftPlot(object, p.cols = c("black", "red"), p.shapes = c(1, 8),
#'               p.sizes = c(1.5, 1.5), s.col = "black",
#'               s.type = 1, s.width = 0.5, max.tick = 50,
#'               ref.lab = "Reference ranking", alt.lab = "Alternative ranking",
#'               y.lab = "Ranking", combine = FALSE, nr = NULL, nc = NULL)
#'
#' @inheritParams rankScatterPlot
#' @param p.cols a vector with two elements denoting the color codes for
#'   reference and alternative positions. See \sQuote{Color Specification} in
#'   [`par`].
#' @param p.shapes a vector with two elements denoting the shapes for reference
#'   and alternative positions. See \sQuote{pch values} in [`points`].
#' @param p.sizes a vector with two elements denoting the sizes for reference
#'   and alternative positions.
#' @param s.col color code for rank shift segments. See \sQuote{Color
#'   Specification} in [`par`].
#' @param s.type line type for rank shift segments. See \sQuote{Line Type
#'   Specification} in [`par`].
#' @param s.width line width for rank shift segments.
#' @param y.lab label of the y-axis.
#'
#' @returns
#' A plot displaying shifts in ranking between two indices. In case `object$time`
#' is not `NULL`, a list of plots for different time factors and the combined
#' grid (if `combine = TRUE`) will be returned. The function does not print the
#' return value if it is assigned to an object. Use [`print`] with the storing
#' object to produce the plot.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @seealso [`rankComp`], [`rankScatterPlot`], [`rankRankPlot`].
#'
#' @example ./inst/examples/plot_eg.R
#'
#' @export
rankShiftPlot <- function(object, p.cols = c("black", "red"), p.shapes = c(1, 8),
                          p.sizes = c(1.5, 1.5), s.col = "black", s.type = 1,
                          s.width = 0.5, max.tick = 50,
                          ref.lab = "Reference ranking",
                          alt.lab = "Alternative ranking",
                          y.lab = "Ranking",
                          combine = FALSE, nr = NULL, nc = NULL) {
  col <- c(p.cols, s.col)
  names(col) <- c(ref.lab, alt.lab, "Shift")
  if (is.null(object$id)) object$id <- 1:nrow(object)
  if (is.null(object$time)) object$time <- as.factor(rep(0, nrow(object)))
  tick <- list()
  p <- list()
  for (t in sort(unique(object$time))) {
    loc <- object$time == t
    tick[[t]] <- tickControl(
      id = object$id[loc][order(object$ref.rank[loc])],
      max.tick = max.tick
    )
    p[[t]] <- ggplot(data = object[loc, ], aes(x = reorder(id, ref.rank))) +
      geom_segment(aes(y = ref.rank, yend = alt.rank, color = "Shift"),
        linetype = s.type, linewidth = s.width
      ) +
      geom_point(aes(y = ref.rank, color = ref.lab),
        shape = p.shapes[1], size = p.sizes[1]
      ) +
      geom_point(aes(y = alt.rank, color = alt.lab),
        shape = p.shapes[2], size = p.sizes[2]
      ) +
      scale_colour_manual(values = col, breaks = names(col)) +
      scale_y_continuous(breaks = tickControl(1:max(
        object$ref.rank,
        object$alt.rank
      ))) +
      scale_x_discrete(breaks = tick[[t]]) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
        axis.title.x = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(title = t, y = y.lab)
  }
  if (length(p) == 1) {
    p <- p[[1]]
    p <- p + labs(title = NULL)
    return(p)
  } else {
    if (isTRUE(combine)) {
      p.comb <- ggpubr::ggarrange(
        plotlist = p, nrow = nr, ncol = nc,
        common.legend = TRUE, legend = "bottom"
      )
      p[["comb"]] <- p.comb
    }
    return(p)
  }
}
