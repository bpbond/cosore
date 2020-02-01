# User-side convenience functions

#' Extract one table (\code{description}, \code{data}, etc.) from datasets
#'
#' @param table Name of table to extract, character
#' @param datasets Character vector of dataset names, e.g. from \code{\link{list_datasets}}
#' @param quiet Print progress messages and warnings? Logical
#' @return A \code{\link{data.frame}}.
#' @export
csr_table <- function(table, datasets = list_datasets(), quiet = FALSE) {

  if(missing(table)) {
    stop("'table' should be one of 'description', 'contributors',",
         "'ports', 'columns', 'data', 'diagnostics', or 'ancillary'")
  }

  extract <- function(dataset_name, table, quiet) {
    mdo <- !table %in% c("data", "diagnostics") # only these require actual data read (which is slow)
    x <- csr_dataset(dataset_name, metadata_only = mdo, quiet = quiet)

    if(is.null(x[[table]])) { return(NULL) }
    if(!is.data.frame(x[[table]])) { return(NULL) }
    if(nrow(x[[table]]) == 0) { return(NULL) }

    x[[table]]$CSR_DATASET <- x$description$CSR_DATASET
    x[[table]]
  }

  rbind_list(lapply(datasets, extract, table, quiet))
}

#' Return a single COSORE dataset.
#'
#' @param dataset_name The dataset name (from \code{\link{list_datasets}}), character
#' @param quiet Print progress messages and warnings? Logical
#' @param metadata_only Quick-read metadata only? Logical
#' @return A list with (at least) elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file. This contains site,
#' instrument, and publication information.}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file with dataset contributor
#' information. The first contributor listed is assumed to the main contact.}
#' \item{ports}{Contents of \code{PORTS.txt} file, describing the species and measurement
#' conditions of different ports (typically multiple chambers are measured by a single
#' gas analyzer).}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}.}
#' \item{diagnostics}{Diagnostics on the data parsing and QC process.}
#' \item{ancillary}{Ancillary site information.}
#' @export
csr_dataset <- function(dataset_name, quiet = FALSE, metadata_only = FALSE) {
  stopifnot(is.character(dataset_name))
  stopifnot(length(dataset_name) == 1)
  stopifnot(is.logical(quiet))
  stopifnot(is.logical(metadata_only))

  # no raw data parsing allowed
  read_dataset(dataset_name, force_raw = FALSE, quiet = quiet, metadata_only = metadata_only)
}

#' Return metadata for the entire COSORE database.
#'
#' @return A \code{data.frame} with metadata about each constituent dataset. This consists of parts of
#' the \code{description} file, joined with parts of \code{ports} and \code{diag}.
#' @export
#' @importFrom stats aggregate
csr_database <- function() {
  desc <- csr_table("description")
  desc <- desc[c("CSR_DATASET", "CSR_LONGITUDE", "CSR_LATITUDE", "CSR_ELEVATION", "CSR_IGBP")]

  ports <- csr_table("ports")
  ports <- aggregate(CSR_MSMT_VAR ~ CSR_DATASET, data = ports, FUN = function(x) paste(unique(x), collapse = ", "))

  diag <- csr_table("diagnostics", quiet = TRUE)
  diag <- diag[c("CSR_DATASET", "CSR_RECORDS", "CSR_TIME_BEGIN", "CSR_TIME_END")]

  x <- merge(desc, diag, by = "CSR_DATASET", all.x = TRUE)
  tibble::as_tibble(merge(x, ports, by = "CSR_DATASET", all.x = TRUE))
}
