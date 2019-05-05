

#' Parse a custom file from d20190504_DAVIDSON_hf006-05.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
`parse_LI-6252_d20190504_DAVIDSON_hf006-05` <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))

  dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%dT%H:%M", tz = "UTC") - UTC_offset * 60 * 60

  dat_control <- subset(dat, treatment == "C")
  dat_control$CSR_T10 <- dat_control$soilt.c
  dat_control$CSR_SM10 <- dat_control$vsm.c
  dat_trench <- subset(dat, treatment != "C")
  dat_trench$CSR_T10 <- dat_trench$soilt.t
  dat_trench$CSR_SM10 <- dat_trench$vsm.t

  rbind_list(list(dat_control, dat_trench))
}


#' Parse a custom file from d20190504_DAVIDSON_hf006-03.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
`parse_LI-6252_d20190504_DAVIDSON_hf006-03` <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))

  dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%dT%H:%M", tz = "UTC") - UTC_offset * 60 * 60

  # Temperature and soil moisture data
  names(dat) <- gsub("^temp\\.ll", "CSR_T0", names(dat))
  names(dat) <- gsub("^temp", "CSR_T", names(dat))
  names(dat) <- gsub("^mois", "CSR_SM", names(dat))

  # Flux fields
  fluxcols <- grep("^flux", names(dat))
  x <- dat[-fluxcols]
  results <- list()
  for(i in seq_along(fluxcols)) {
    results[[i]] <- x
    results[[i]]$flux <- dat[,fluxcols[i]]
    results[[i]]$CSR_PORT <- i
  }

  rbind_list(results)
}

#' Parse a custom eofFD (forced diffusion) file from d20190430_DESAI.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
parse_EOSFD_d20190430_DESAI <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))

  dat$Time.UTC <- as.POSIXct(dat$Time.UTC, format = "%m/%d/%y %H:%M", tz = "UTC")

  results <- list()
  for(p in 1:4) {   # four separate ports in the file
    p_chr <- paste0("P", p)
    x <- dat[c("Time.UTC")]
    x$CSR_FLUX <- dat[,paste0("QCCombo.Flux.", p_chr)]
    x$CSR_ERROR <- x$CSR_FLUX > 50  # ad hoc
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

  rbind_list(results)
}


#' Parse a file from Harvard Forest HF068 dataset.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
`parse_LI-820_d20190415_VARNER` <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv, stringsAsFactors = FALSE, check.names = FALSE))
  dat$datetime <- as.POSIXct(dat$datetime, format = "%Y-%m-%dT%H:%M", tz = "UTC") - UTC_offset * 60 * 60
  dat$CSR_ERROR <- FALSE
  dat
}

#' Parse a file with timestamp in year-doy-hour format.
#'
#' @param path Data directory path, character
#' @param UTC_offset Offset from UTC in hours, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @export
`parse_LI-8100_YEARDOYHOUR` <- function(path, UTC_offset) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.csv,
                                 na.strings = "-9999", stringsAsFactors = FALSE, check.names = FALSE))
  dat$Timestamp <- as.POSIXct(paste(dat$year, dat$doy, dat$hour),
                              format = "%Y %j %H", tz = "UTC") - UTC_offset * 60 * 60
  dat$year <- dat$doy <- dat$hour <- NULL
  dat
}
