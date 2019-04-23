# User-side convenience functions

csr_element <- function(all, element) {

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

csr_data <- function(all) {
  csr_element(all, "data")
}
