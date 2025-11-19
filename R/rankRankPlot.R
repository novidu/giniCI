#' Rank-rank Plot
#'
#' Generate rank-rank plots for ranking comparison.
#'
#' @usage
#' rankRankPlot(object, id.col = FALSE, p.size = 1.5, p.nudge = 0.05,
#'              s.width = 0.5,  lab.size = 3.88,
#'              max.overlaps = 10, max.tick = 50,
#'              ref.lab = "Reference ranking", alt.lab = "Alternative ranking",
#'              y.lab = NULL, combine = FALSE, nr = NULL, nc = NULL)
#'
#' @inheritParams rankScatterPlot
#' @param id.col a logical value indicating whether the rank-rank segment should
#'   be colored by unit identifiers. It is not recommended if having more than
#'   20 units.
#' @param p.size size of segment endpoints.
#' @param p.nudge horizontal adjustment value to nudge the starting position of
#'   labels.
#' @param s.width line width for rank-rank segments.
#' @param y.lab label of the y-axis.
#'
#' @returns
#' A plot comparing two rankings connected by segments. In case `object$time`
#' is not `NULL`, a list of plots for different time factors and the combined
#' grid (if `combine = TRUE`) will be returned. The function does not print the
#' return if it is assigned to an object. Use [`print`] with the storing object
#' to generate the plot.
#'
#' @author
#' Viet Duong Nguyen, Chiara Gigliarano, Mariateresa Ciommi
#'
#' @seealso [`rankComp`], [`rankScatterPlot`], [`rankShiftPlot`].
#'
#' @example ./inst/examples/plot_eg.R
#'
#' @export
rankRankPlot <- function(object, id.col = FALSE, p.size = 1.5, p.nudge = 0.05,
                         s.width = 0.5, lab.size = 3.88,
                         max.overlaps = 10, max.tick = 50,
                         ref.lab = "Reference ranking",
                         alt.lab = "Alternative ranking",
                         y.lab = NULL,
                         combine = FALSE, nr = NULL, nc = NULL) {
  if (is.null(object$id)) object$id <- 1:nrow(object)
  if (is.null(object$time)) object$time <- as.factor(rep(0, nrow(object)))
  p <- list()
  for (t in sort(unique(object$time))) {
    loc <- object$time == t
    object2 <- rbind(
      data.frame(
        id = object[loc, ]$id,
        rank = object[loc, ]$ref.rank, type = ref.lab
      ),
      data.frame(
        id = object[loc, ]$id,
        rank = object[loc, ]$alt.rank, type = alt.lab
      )
    )
    max.rank <- max(object2$rank, na.rm = T)
    p[[t]] <- ggplot(
      data = object2,
      aes(
        x = factor(type, levels = c(ref.lab, alt.lab)),
        y = rank, group = id
      )
    )
    if (isTRUE(id.col)) {
      p[[t]] <- p[[t]] +
        geom_line(aes(color = id), linewidth = s.width) +
        geom_point(aes(color = id), size = p.size)
    } else {
      p[[t]] <- p[[t]] +
        geom_line(linewidth = s.width) +
        geom_point(size = p.size)
    }
    p[[t]] <- p[[t]] +
      scale_y_continuous(
        trans = "reverse",
        breaks = tickControl(max.rank:1, max.tick = max.tick)
      ) +
      theme(
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = t, y = y.lab) +
      ggrepel::geom_text_repel(aes(label = id),
        segment.linetype = 2,
        data = object2[object2$type == ref.lab, ],
        size = lab.size, nudge_x = -p.nudge,
        direction = "y", hjust = 1, vjust = 0.5,
        max.time = 10,
        max.overlaps = max.overlaps
      ) +
      ggrepel::geom_text_repel(aes(label = id),
        segment.linetype = 2,
        data = object2[object2$type == alt.lab, ],
        size = lab.size, nudge_x = p.nudge,
        direction = "y", hjust = 0, vjust = 0.5,
        max.time = 10,
        max.overlaps = max.overlaps
      )
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
