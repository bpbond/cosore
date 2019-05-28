# User-side convenience functions

#' Extract one table (\code{description}, \code{data}, etc.) of all datasets
#'
#' @param all All data, a list of lists
#' @param table Name of table to extract, character
#' @return A \code{\link{data.frame}}.
#' @export
csr_table <- function(all, table) {

  extract <- function(x, table) {
    stopifnot(table %in% names(x))
    x[[table]]$CSR_DATASET <- x$dataset_name
    x[[table]]
  }

  rbind_list(lapply(all, extract, table = table))
}

#' Extract all the \code{data} tables from the COSORE dataset
#'
#' @param all All data, a list of lists
#' @return A \code{\link{data.frame}}.
#' @export
csr_data <- function(all) {
  csr_table(all, "data")
}

#' Extract all the \code{description} tables from the COSORE dataset
#'
#' @param all All data, a list of lists
#' @return A \code{\link{data.frame}}.
#' @export
csr_description <- function(all) {
  csr_table(all, "description")
}
