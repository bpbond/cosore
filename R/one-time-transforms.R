
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
    if(new %in% names(dsd)) {
      stop(new, " already exists in data!")
    }
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
      stopifnot(file.exists(outfile))
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


# A variety of mostly minor tweaks laid out in #197
dc197 <- function() {
  file.remove("~/Desktop/par.txt")
  file.remove("~/Desktop/wind.txt")
  file.remove("~/Desktop/precip.txt")
  file.remove("~/Desktop/twater.txt")

  for(ds in list_datasets()) {
    message(ds)
    dsd <- csr_table("data", ds)
    dsd$CSR_DATASET <- NULL
    diag <- csr_table("diagnostics", ds)
    diag$CSR_DATASET <- NULL
    contrib <- csr_table("contributors", ds)

    if(is.data.frame(dsd) & nrow(dsd)) {
      # Rename dataset fields
#      dsd <- rename(dsd, "CSR_O2", "CSR_SOIL_O2")
      if("CSR_NEE" %in% names(dsd)) {
        message("\tNEE needs moving to ancillary")
        write.csv(dsd[c("CSR_TIMESTAMP_BEGIN", "CSR_TIMESTAMP_END", "CSR_PORT", "CSR_NEE")],
                  file.path("~/Desktop/", paste0(ds, "_nee.csv")), row.names = FALSE)
        dsd$CSR_NEE <- NULL
      }
      if("CSR_PAR" %in% names(dsd)) {
        message("\tPAR needs checking")
        cat(ds, contrib$CSR_EMAIL[1], "\n", sep = "\t", file = "~/Desktop/par.txt", append = TRUE)
      }
      if("CSR_PRECIP" %in% names(dsd)) {
        message("\tPRECIP needs checking")
        cat(ds, contrib$CSR_EMAIL[1], "\n", sep = "\t", file = "~/Desktop/precip.txt", append = TRUE)
      }
      dsd$CSR_PORT <- as.integer(dsd$CSR_PORT)
      if("CSR_RECORD" %in% names(dsd)) {
        dsd$CSR_RECORD <- as.integer(dsd$CSR_RECORD)
      }
      if("CSR_WIND" %in% names(dsd)) {
        message("\tWIND needs checking")
        cat(ds, contrib$CSR_EMAIL[1], "\n", sep = "\t", file = "~/Desktop/wind.txt", append = TRUE)
      }
      # dsd <- rename(dsd, "CSR_TAIR", "CSR_TAIR_AMB")
      # dsd <- rename(dsd, "CSR_TCHAMBER", "CSR_TAIR")
      if("CSR_TWATER" %in% names(dsd)) {
        message("\tTWATER needs checking")
        cat(ds, contrib$CSR_EMAIL[1], "\n", sep = "\t", file = "~/Desktop/twater.txt", append = TRUE)
      }

      outfile <- file.path("./inst/extdata/datasets/", ds, "data", "data.RDS")
      stopifnot(file.exists(outfile))
      saveRDS(dsd, file = outfile)

      # Rename diagnostics
      diag$CSR_RECORDS <- as.integer(diag$CSR_RECORDS)
      diag$CSR_RECORDS_REMOVED_ERR <- as.integer(diag$CSR_RECORDS_REMOVED_ERR)
      diag$CSR_RECORDS_REMOVED_NA <- as.integer(diag$CSR_RECORDS_REMOVED_NA)
      diag$CSR_RECORDS_REMOVED_TIMESTAMP <- as.integer(diag$CSR_RECORDS_REMOVED_TIMESTAMP)
      diag$CSR_REMOVED_HIGH_CO2 <- as.integer(diag$CSR_REMOVED_HIGH_CO2)
      diag$CSR_REMOVED_LOW_CO2 <- as.integer(diag$CSR_REMOVED_LOW_CO2)
      diag$CSR_REMOVED_HIGH_CH4 <- as.integer(diag$CSR_REMOVED_HIGH_CH4)
      diag$CSR_REMOVED_LOW_CH4 <- as.integer(diag$CSR_REMOVED_LOW_CH4)

      outfile <- file.path("./inst/extdata/datasets/", ds, "data", "diag.RDS")
      stopifnot(file.exists(outfile))
      saveRDS(diag, file = outfile)

      # Remove CSR_PORT from ancillary files and add CSR_DATE
      anc_file <- file.path("inst/extdata/datasets/", ds, "ANCILLARY.csv")
      anc <- read.csv(anc_file, stringsAsFactors = FALSE)
      if(!"CSR_DATE" %in% names(anc)) {
        if(nrow(anc)) {
          anc$CSR_DATE <- NA
        } else {
          anc <- cbind(anc, data.frame(CSR_DATE = integer()))
        }
      }
      if("CSR_PORT" %in% names(anc)) {
        anc$CSR_PORT <- NULL
      }
      write.csv(anc, file = anc_file, row.names = FALSE, quote = FALSE, na = "")
    }
  }
}
