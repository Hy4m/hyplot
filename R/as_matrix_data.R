#' Coerce to a matrix_data
#' @title as_matrix_data
#' @param x any \code{R} object.
#' @param ... ignore.
#' @return a matrix_data object.
#' @rdname as_matrix_data
#' @author Hou Yun
#' @export
as_matrix_data <- function(x, ...)
{
  UseMethod("as_matrix_data")
}

#' @rdname as_matrix_data
#' @export
#' @method as_matrix_data matrix
as_matrix_data.matrix <- function(x, name = NULL, ...)
{
  x <- list(x)
  if (!is.null(name)) {
    names(x) <- name
  }
  matrix_data(x)
}

#' @param include one of "numeric" (default), "character" or "factor".
#' @rdname as_matrix_data
#' @export
#' @method as_matrix_data data.frame
as_matrix_data.data.frame <- function(x,
                                      name = NULL,
                                      include = "numeric",
                                      ...)
{
  include <- match.arg(include, c("numeric", "character", "factor"))
  Fun <- paste0("is.", include)
  id <- vapply(x, Fun, logical(1))
  x <- list(x[id])
  if (!is.null(name)) {
    names(x) <- name
  }
  matrix_data(x)
}

#' @rdname as_matrix_data
#' @export
#' @method as_matrix_data correlate
as_matrix_data.correlate <- function(x, ...) {
  id <- vapply(x, is.null, logical(1))
  x <- x[!id]
  matrix_data(x)
}

#' @rdname as_matrix_data
#' @export
#' @method as_matrix_data default
as_matrix_data.default <- function(x, ...)
{
  stop("Unknown data type.", call. = FALSE)
}
