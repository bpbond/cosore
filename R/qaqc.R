

#' QA/QC a dataset.
#'
#' @param dsd Dataset data, a tibble
#' @param diag Diagnostics data, a tibble
#' @param remove_na Remove NA flux values? Logical
#' @param remove_error Remove records marked as errors? Logical
#' @return A list with the `dsd` and `diag` tibbles.
#' @importFrom tibble is_tibble
#' @note This is normally called from \code{\link{read_raw_dataset}}.
#' @keywords internal
qaqc_data <- function(dsd, diag,
                      remove_na = TRUE,
                      remove_error = TRUE) {

  stopifnot(is_tibble(dsd))
  stopifnot(is_tibble(diag))
  stopifnot(is.logical(remove_na))
  stopifnot(is.logical(remove_error))

  # Remove NA flux records
  diag$CSR_RECORDS_REMOVED_NA <- 0
  if(remove_na) {
    if("CSR_FLUX_CH4" %in% names(dsd)) {
      na_flux <- is.na(dsd$CSR_FLUX_CO2) & is.na(dsd$CSR_FLUX_CH4)
    } else {
      na_flux <- is.na(dsd$CSR_FLUX_CO2)
    }
    diag$CSR_RECORDS_REMOVED_NA <- sum(na_flux)
    dsd <- dsd[!na_flux,]
  }

  # Remove error records
  diag$CSR_RECORDS_REMOVED_ERR <- 0L
  if(remove_error & "CSR_ERROR" %in% names(dsd)) {
    err <- dsd$CSR_ERROR
    diag$CSR_RECORDS_REMOVED_ERR <- sum(err)
    dsd <- dsd[!err,]
    dsd$CSR_ERROR <- NULL
  }

  diag$CSR_RECORDS <- nrow(dsd)

  list(dsd = dsd, diag = diag)
}
