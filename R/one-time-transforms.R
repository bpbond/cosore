
# A utility function that I may need again sometime, if something gets added to diags
# and I don't want to re-run csr_build()
redo_diagnostics <- function() {
  for(ds in list_datasets()) {
    message(ds)
    dsd <- csr_table("data", ds)
    diag <- csr_table("diagnostics", ds)

    if(is.data.frame(dsd) & nrow(dsd)) {
      # Diagnostic information
      diag$CSR_DATASET <- NULL  # this is added by csr_table(); remove
      diag$CSR_TIME_BEGIN <- format(min(dsd$CSR_TIMESTAMP_BEGIN), format = "%Y-%m-%d")
      diag$CSR_TIME_END <- format(max(dsd$CSR_TIMESTAMP_END), format = "%Y-%m-%d")
      outfile <- file.path("./inst/extdata/datasets/", ds, "data", "diag.RDS")
      stopifnot(file.exists(outfile))
      saveRDS(diag, file = outfile)
    }
  }
}

rename <- function(dsd, old, new) {
  if(old %in% names(dsd)) {
    names(dsd)[names(dsd) == old] <- new
  }
  dsd
}

add_ch4 <- function() {
  for(ds in list_datasets()) {
    message(ds)
    dsd <- csr_table("data", ds)
    dsd$CSR_DATASET <- NULL
    diag <- csr_table("diagnostics", ds)
    diag$CSR_DATASET <- NULL

    if(is.data.frame(dsd) & nrow(dsd)) {
      # Rename dataset fields
      dsd <- rename(dsd, "CSR_FLUX", "CSR_FLUX_CO2")
      dsd <- rename(dsd, "CSR_CDRY", "CSR_DRY_CO2")
      dsd <- rename(dsd, "CSR_CO2_AMB", "CSR_AMB_CO2")
      dsd <- rename(dsd, "CSR_CRVFIT", "CSR_CRVFIT_CO2")
      dsd <- rename(dsd, "CSR_FLUX_SE", "CSR_FLUX_SE_CO2")
      dsd <- rename(dsd, "CSR_R2", "CSR_R2_CO2")

      outfile <- file.path("./inst/extdata/datasets/", ds, "data", "data.RDS")
      saveRDS(dsd, file = outfile)

      # Rename diagnostics
      diag <- rename(diag, "CSR_FLUX_LOWBOUND", "CSR_FLUX_LOW_LIM_CO2")
      diag <- rename(diag, "CSR_FLUX_HIGHBOUND", "CSR_FLUX_HIGH_LIM_CO2")
      diag$CSR_FLUX_LOW_LIM_CH4 <- NA
      diag$CSR_FLUX_HIGH_LIM_CH4 <- NA
      diag <- rename(diag, "CSR_RECORDS_REMOVED_TOOLOW", "CSR_REMOVED_LOW_CO2")
      diag <- rename(diag, "CSR_RECORDS_REMOVED_TOOHIGH", "CSR_REMOVED_HIGH_CO2")
      diag$CSR_REMOVED_LOW_CH4 <- 0
      diag$CSR_REMOVED_HIGH_CH4 <- 0

      outfile <- file.path("./inst/extdata/datasets/", ds, "data", "diag.RDS")
      stopifnot(file.exists(outfile))
      saveRDS(diag, file = outfile)
    }
  }
}
