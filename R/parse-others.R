
#' Parse a custom eofFD (forced diffusion) file from d20190430_DESAI.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
parse_eosFD_Desai <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))

  dat$Time.UTC <- as.POSIXct(dat$Time.UTC, format = "%m/%d/%y %H:%M", tz = "UTC")
  dat$Area <- pi * (10 / 2) ^ 2
  dat$Error <- FALSE

  results <- list()
  for(p in 1:4) {
    p_chr <- paste0("P", p)
    x <- dat[c("Time.UTC", "Area", "Error")]
    x$CSR_FLUX <- dat[,paste0("QCCombo.Flux.", p_chr)]
    x$CSR_PORT <- p

    # Extract port-specific temperature at various depths...
    temps <- grep(paste0("^Ts.*", p_chr), names(dat))
    x <- cbind(x, dat[temps])
    names(x) <- gsub(paste0("Ts\\.", p_chr, "\\."), "CSR_T", names(x))

    # ...and moisture
    sm <- grep(paste0("^VWC.*", p_chr), names(dat))
    x <- cbind(x, dat[sm])
    names(x) <- gsub(paste0("VWC\\.", p_chr, "\\."), "CSR_SM", names(x))

    results[[p_chr]] <- x
  }

  rbind_all(results)
}


#' Parse a file from Harvard Forest HF068 dataset.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
parse_d20190415_HF068 <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))
  dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%dT%H:%M", tz = "UTC") - UTC_offset * 60 * 60
  dat$year <- dat$doy <- dat$hour <- NULL
  # Fill in data given at http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf068
  dat$Area <- 43.2 ^ 2
  dat$Error <- FALSE
  dat
}

#' Parse a file with timestamp in year-doy-hour format.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
parse_IRGA_ydh <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv,
                                 na.strings = "-9999", stringsAsFactors = FALSE, check.names = FALSE))
  dat$Timestamp <- as.POSIXct(paste(dat$year, dat$doy, dat$hour),
                              format = "%Y %j %H", tz = "UTC") - UTC_offset * 60 * 60
  dat$year <- dat$doy <- dat$hour <- NULL
  # Fill in data given at https://www.sciencedirect.com/science/article/pii/S0168192312003759
  dat$Area <- 491
  dat$Error <- FALSE
  dat
}
