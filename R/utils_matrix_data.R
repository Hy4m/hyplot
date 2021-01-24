#' Helper function for matrix_data object
#' @title Helper function
#' @param md a matrix_data object.
#' @param value a valid value for dimension names.
#' @rdname Helper_function
#' @author Hou Yun
#' @export
row_names <- function(md)
{
  if (is_matrix_data(md)) {
    rownames(md[[1]])
  } else if (is_md_tbl(md)) {
    attr(md, "row_names")
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
`row_names<-` <- function(md, value)
{
  if (is_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      rownames(.md) <- value
      .md
    })
    structure(.Data = md, class = "matrix_data")
  } else if (is_md_tbl(md)) {
    attr(md, "row_names") <- value
    md
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
col_names <- function(md)
{
  if (is_matrix_data(md)) {
    colnames(md[[1]])
  } else if (is_md_tbl(md)) {
    attr(md, "col_names")
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
`col_names<-` <- function(md, value)
{
  if (is_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      colnames(.md) <- value
      .md
    })
    structure(.Data = md, class = "matrix_data")
  } else if (is_md_tbl(md)) {
    attr(md, "col_names") <- value
    md
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
nrows <- function(md) {
  if (is_matrix_data(md)) {
    nrow(md[[1]])
  } else if (is_md_tbl(md)) {
    length(attr(md, "row_names"))
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
ncols <- function(md) {
  if (is_matrix_data(md)) {
    ncol(md[[1]])
  } else if (is_md_tbl(md)) {
    length(attr(md, "col_names"))
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
is_matrix_data <- function(md)
{
  inherits(md, "matrix_data")
}

#' @rdname Helper_function
#' @export
is_md_tbl <- function(md)
{
  inherits(md, "md_tbl")
}
