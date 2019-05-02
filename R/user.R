# User-side convenience functions

#' Extract one part (\code{description}, \code{data}, etc.) of all datasets.
#'
#' @param all All data, a list of lists
#' @param element Name of table to extract, character
#' @return A \code{\link{data.frame}}.
#' @export
csr_element <- function(all, element) {

  # We want to rbind() the various data frames together, but column
  # names may differ (for the data tables anyway). Need to handle this.
  # Almost all of this could be replaced by a single `dplyr::bind_rows()` call...

  all_names <- unique(unlist(lapply(all, function(x) names(x[[element]]))))

  extract <- function(x, element) {
    x[[element]]$CSR_DATASET <- x$dataset_name
    x[[element]]
  }
  do.call(rbind,
          c(lapply(all, function(x) {
            xel <- extract(x, element)
            data.frame(c(xel, sapply(setdiff(all_names, names(xel)),
                                     function(y) NA)),
                       check.names = FALSE, stringsAsFactors = FALSE)
          }), make.row.names = FALSE, stringsAsFactors = FALSE))
}

#' Extract all the \code{data} tables from the COSORE dataset.
#'
#' @param all All data, a list of lists
#' @return A \code{\link{data.frame}}.
#' @export
csr_data <- function(all) {
  csr_element(all, "data")
}

