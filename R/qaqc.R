

#' QA/QC a dataset.
#'
#' @param dsd Dataset data, a tibble
#' @param diag Diagnostics data, a tibble
#' @param remove_na Remove NA flux values? Logical
#' @param remove_error Remove records marked as errors? Logical
#' @param remove_flux Remove extreme flux values? Logical
#' @param remove_temp Remove extreme temperature values? Logical
#' @param flux_limits_co2 Two-value numeric vector giving CO2 flux limits
#' @param temp_limits Two-value numeric vector giving temperature limits
#' @return A list with the `dsd` and `diag` tibbles.
#' @importFrom tibble is_tibble
#' @note This is normally called from \code{\link{read_raw_dataset}}.
#' @keywords internal
qaqc_data <- function(dsd, diag,
                      remove_na = TRUE,
                      remove_error = TRUE,
                      remove_flux = TRUE,
                      remove_temp = TRUE,
                      flux_limits_co2 = c(-10, 50),
                      temp_limits = c(-50, 60)) {

  stopifnot(is_tibble(dsd))
  stopifnot(is_tibble(diag))
  stopifnot(is.logical(remove_na))
  stopifnot(is.logical(remove_error))
  stopifnot(is.logical(remove_flux))
  stopifnot(is.logical(remove_temp))
  stopifnot(is.numeric(flux_limits_co2))
  stopifnot(is.numeric(temp_limits))
  stopifnot(length(flux_limits_co2) == 2)
  stopifnot(length(temp_limits) == 2)

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
  diag$CSR_RECORDS_REMOVED_ERR <- 0
  if(remove_error & "CSR_ERROR" %in% names(dsd)) {
    err <- dsd$CSR_ERROR
    diag$CSR_RECORDS_REMOVED_ERR <- sum(err)
    dsd <- dsd[!err,]
    dsd$CSR_ERROR <- NULL
  }

  # Remove records with flux data way out of anything possible
  if(remove_flux) {
    diag$CSR_FLUX_LOW_LIM_CO2 <- min(flux_limits_co2)
    diag$CSR_FLUX_HIGH_LIM_CO2 <- max(flux_limits_co2)
    toolow <- dsd$CSR_FLUX_CO2 < min(flux_limits_co2)
    diag$CSR_REMOVED_LOW_CO2 <- sum(toolow, na.rm = TRUE)
    toohigh <- dsd$CSR_FLUX_CO2 > max(flux_limits_co2)
    diag$CSR_REMOVED_HIGH_CO2 <- sum(toohigh, na.rm = TRUE)
    # Need to replace NAs with FALSE so we don't nuke data!
    toolow[is.na(toolow)] <- FALSE
    toohigh[is.na(toohigh)] <- FALSE
    dsd <- dsd[!toolow & !toohigh,]
  } else {
    diag$CSR_FLUX_LOW_LIM_CO2 <- NA
    diag$CSR_FLUX_HIGH_LIM_CO2 <- NA
    diag$CSR_REMOVED_LOW_CO2 <- 0
    diag$CSR_REMOVED_HIGH_CO2 <- 0
  }

  diag$CSR_RECORDS <- nrow(dsd)

  # Remove bad temperature values
  diag$CSR_BAD_TEMPERATURE <- 0
  if(remove_temp) {
    for(tmp in c("CSR_TCHAMBER", "CSR_T5")) {
      if(tmp %in% names(dsd) && nrow(dsd)) {
        dsd[,tmp] <- convert_to_numeric(unlist(dsd[tmp]), tmp)

        tmpvals <- dsd[tmp]
        bad_temps <- tmpvals < min(temp_limits) | tmpvals > max(temp_limits)
        bad_temps[is.na(bad_temps)] <- FALSE
        dsd[tmp][bad_temps] <- NA  # NA out bad values
        diag$CSR_BAD_TEMPERATURE <- diag$CSR_BAD_TEMPERATURE +
          sum(bad_temps, na.rm = TRUE)
      }
    }
  }

  list(dsd = dsd, diag = diag)
}
