# User-side convenience functions

#' Extract one table (\code{description}, \code{data}, etc.) of all datasets
#'
#' @param all All data, a list of lists
#' @param table Name of table to extract, character
#' @return A \code{\link{data.frame}}.
#' @export
csr_table <- function(all, table) {

  extract <- function(x, table) {
    if(is.null(x[[table]])) { return(NULL) }

    x[[table]]$CSR_DATASET <- x$description$CSR_DATASET
    x[[table]]
  }

  rbind_list(lapply(all, extract, table = table))
}
