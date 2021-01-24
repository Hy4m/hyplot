#' Matrix data
#' @title Matrix data
#' @param x a list of matrix-like data.
#' @param row_names,col_names the name of rows and columns.
#' @return a object of matrix_data
#' @rdname matrix_data
#' @author Hou Yun
#' @export
matrix_data <- function(x,
                        row_names = NULL,
                        col_names = NULL)
{
  stopifnot(is.list(x))
  nm <- names(x)
  if (length(x) != length(unique(nm))) {
    stop("matrix_data error: each element of `x` must have a unique name.", call. = FALSE)
  }
  mat <- lapply(x, as.matrix)
  mat <- check_matrix(mat, row_names, col_names)

  structure(.Data = mat, class = "matrix_data")
}

#' @method print matrix_data
#' @export
print.matrix_data <- function(x, ...) {
  dims <- dim(x[[1]])
  cat("Number: ", length(x), "\n")
  cat("Dimensions: ", paste0(dims[1], " rows, ", dims[2], " columns.\n"))
  cat("Row names: ", glue::glue_collapse(rownames(x[[1]]), ", ", 60), "\n")
  cat("Column names: ", glue::glue_collapse(colnames(x[[1]]), ", ", 60), "\n")
}

check_matrix <- function(mat,
                         row_names = NULL,
                         col_names = NULL)
{
  n <- length(mat)
  if (n == 0L || (nrow(mat[[1]]) == 0L) || (ncol(mat[[1]]) == 0L)) {
    stop("The input data is empty.", call. = FALSE)
  }

  first <- mat[[1]]
  if (length(unique(unlist(lapply(mat, nrow)))) != 1L) {
    stop("The rows of input matrix should be same.", call. = FALSE)
  }
  if (length(unique(unlist(lapply(mat, ncol)))) != 1L) {
    stop("The rows of input matrix should be same.", call. = FALSE)
  }
  mat <- lapply(mat, function(x) {
    if (!is.null(row_names)) {
      rownames(x) <- row_names
    } else {
      if (is.null(rownames(x))) {
        rownames(x) <- rownames(first) %||% paste0("Row", seq_len(nrow(first)))
      }
    }
    if (!is.null(col_names)) {
      colnames(x) <- col_names
    } else {
      if (is.null(colnames(x))) {
        colnames(x) <- colnames(first) %||% paste0("Col", seq_len(ncol(first)))
      }
    }
    x
  })

  l_row_names <- lapply(mat, rownames)
  l_col_names <- lapply(mat, colnames)
  if (!all(vapply(l_row_names, identical, logical(1), y = l_row_names[[1]]))) {
    stop("Invalid rownames.", call. = FALSE)
  }

  if (!all(vapply(l_col_names, identical, logical(1), y = l_col_names[[1]]))) {
    stop("Invalid colnames.", call. = FALSE)
  }

  mat
}

set_row_names <- function(x, name) {
  rownames(x) <- name
  x
}

set_col_names <- function(x, name) {
  colnames(x) <- name
  x
}
