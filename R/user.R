# User-side convenience functions

#' Extract one part (\code{description}, \code{data}, etc.) of all datasets.
#'
#' @param all All data, a list of lists
#' @param element Name of table to extract, character
#' @return A \code{\link{data.frame}}.
#' @export
csr_element <- function(all, element) {

  extract <- function(x, element) {
    x[[element]]$CSR_DATASET <- x$dataset_name
    x[[element]]
  }

  rbind_list(lapply(all, extract, element = element))
}

#' Extract all the \code{data} tables from the COSORE dataset.
#'
#' @param all All data, a list of lists
#' @return A \code{\link{data.frame}}.
#' @export
csr_data <- function(all) {
  csr_element(all, "data")
}

#' Extract all the \code{description} tables from the COSORE dataset.
#'
#' @param all All data, a list of lists
#' @return A \code{\link{data.frame}}.
#' @export
csr_description <- function(all) {
  csr_element(all, "description")
}
