#' Coerce matrix_data to data frames
#' @title Coerce matrix_data to data frames
#' @param md a matrix_data object.
#' @param ... passing to \code{\link{make_cluster}}.
#' @return a tibble object.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @rdname as_tibble
#' @author Hou Yun
#' @export
as_tibble.matrix_data <- function(md, ...)
{
  md <- make_cluster(md, ...)
  value <- new_data_frame(lapply(md, as.vector))
  id <- new_data_frame(list(.rownames = rep(row_names(md), ncols(md)),
                            .colnames = rep(col_names(md), each = nrows(md))))
  structure(.Data = bind_cols(id, value),
            row_names = row_names(md),
            col_names = col_names(md),
            class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
}
