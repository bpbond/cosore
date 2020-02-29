
#' Read processed data in whitespace-delimited format
#'
#' @param path Data directory path, character
#' @return A data frame with all data read from file(s).
#' @note Processed (in the Licor application) data consists of a tab-delimited
#' text file with a standard set of columns.
#' @importFrom utils read.delim
#' @keywords internal
parse_PROCESSED <- function(path) {
  files <- list.files(path, pattern = ".(txt|csv)$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.delim,
                                 na.strings = c("NA", "-9999", "#VALUE!", "#REF!"),
                                 stringsAsFactors = FALSE,
                                 strip.white = TRUE,
                                 check.names = FALSE))
  dat$CSR_ERROR <- FALSE
  dat
}

#' Read processed data in CSV format
#'
#' @param path Data directory path, character
#' @return A data frame with all data read from file(s).
#' @note Processed (in the Licor application) data consists of a tab-delimited
#' text file with a standard set of columns.
#' @importFrom utils read.csv
#' @keywords internal
parse_PROCESSED_CSV <- function(path) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- rbind_list(lapply(files, read.csv,
                           na.strings = c("NA", "-9999", "#VALUE!", "#REF!"),
                           stringsAsFactors = FALSE,
                           check.names = FALSE))
  dat$CSR_ERROR <- FALSE
  dat
}
