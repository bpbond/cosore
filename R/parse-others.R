
#' Parse a eofFD (forced diffusion) file.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
parse_eosFD <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".dat$", full.names = TRUE, recursive = TRUE)

  readfunc <- function(f) {
    dat <- readLines(f)
    if(length(dat)) {
      read.csv(textConnection(dat[c(-1, -3, -4)]), stringsAsFactors = FALSE, check.names = FALSE, na.strings = "NAN")
    } else {
      NULL
    }
  }

  dat <- do.call("rbind", lapply(files, readfunc))
  dat$TIMESTAMP <- as.POSIXct(dat$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - UTC_offset * 60 * 60
  dat$Area <- pi * (10 / 2) ^ 2
  dat$Error <- FALSE

  # For now combine all AvgCO2_atm_, etc. columns into a single mean value
  co2cols <- grep("^AvgCO2_atm_", names(dat))
  dat$AvgCO2 <- rowMeans(dat[co2cols], na.rm = TRUE)
  dat[co2cols] <- NULL
  t10cols <- grep("^T107_C_Avg", names(dat))
  dat$T10 <- rowMeans(dat[t10cols], na.rm = TRUE)
  dat[t10cols] <- NULL
  smcols <- grep("^VWC_Avg", names(dat))
  dat$SM10 <- rowMeans(dat[smcols], na.rm = TRUE)
  dat[smcols] <- NULL

  # Treat each Flux_ column as a different port?
  fluxcols <- grep("^Flux_", names(dat))
  x <- dat[-fluxcols]
  results <- list()
  for(i in seq_along(fluxcols)) {
    x$Flux <- dat[,fluxcols[i]]
    x$Port <- i
    results[[i]] <- x
  }
  do.call(rbind, results)
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
