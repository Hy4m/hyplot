#' Initialize a new hyplot
#' @title Initialize hyplot
#' @param md a matrix_data or md_tbl object or any can be converted to matrix_data.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param ... passing to \code{\link{as_matrix_data}}.
#' @return a ggplot object.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom utils modifyList
#' @rdname hyplot
#' @examples
#' library(ggplot2)
#' hyplot(mtcars) +
#'   geom_tile(aes(fill = mtcars))
#' @author Hou Yun
#' @export
hyplot <- function(md, mapping = NULL, ...) {
  if (!is_matrix_data(md) && !is_md_tbl(md)) {
    if (!"name" %in% names(list(...))) {
      nm <- deparse(substitute(md))
      md <- as_matrix_data(md, name = nm, ...)
    } else {
      md <- as_matrix_data(md, ...)
    }
  }

  if (is_matrix_data(md)) {
    md <- fortify(md, ...)
  }

  base_mapping <- aes_(x = ~.colnames, y = ~.rownames)
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  p <- ggplot(data = md,
              mapping = mapping)
  p <- p + scale_x_discrete(limits = col_names(md)) +
           scale_y_discrete(limits = rev(row_names(md)))

  # adjust the default theme
  p <- p + theme_hy()
  class(p) <- c("hyplot", class(p))
  p
}

#' @importFrom ggplot2 coord_polar
hyplot_circular <- function(md,
                            mapping = NULL,
                            open = 45,
                            inner = 1,
                            outer = 0.2,
                            ...) {
  if (!is_matrix_data(md) && !is_md_tbl(md)) {
    if (!"name" %in% names(list(...))) {
      nm <- deparse(substitute(md))
      md <- as_matrix_data(md, name = nm, ...)
    } else {
      md <- as_matrix_data(md, ...)
    }
  }

  if (is_matrix_data(md)) {
    md <- fortify(md, ...)
  }

  base_mapping <- aes_(x = ~.colnames, y = ~.rownames)
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  p <- ggplot(data = md,
              mapping = mapping)
  p <- p + scale_x_discrete(limits = col_names(md)) +
    scale_y_discrete(limits = rev(row_names(md))) +
    coord_polar()

  # adjust the default theme
  p <- p + theme_hy()
  class(p) <- c("hyplot", class(p))
  p
}
