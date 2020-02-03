# Some datasets need custom processing

#' Parse a custom file from d20190830_LIANG
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
#' @details This is a complicated one: the authors posted their data on
#' Figshare (good), but provide only raw CO2 concentration data.
#' Need to read each file, compute the flux for each
#' chamber at each timestep, and stitch together. This is slow.
#' @note One of the files had a corrupted character: in
#' \code{Miyazaki_Efflux_txt_Stage_2/Efflux_2/Miyazaki201205.dat} had
#' to change a stray "(" to "," line 365560. This is noted in the
#' README in the raw data folder.
#' @importFrom stats lm
#' @importFrom utils head
#' @importFrom utils tail write.table
parse_d20190830_LIANG <- function(path) {
  files <- list.files(path, pattern = ".dat$", full.names = TRUE, recursive = TRUE)
  co2files <- grep("Environ", files, invert = TRUE)
  results <- list()

  for(f in seq_along(co2files)) {
    fn <- files[co2files][f]
    #    message(f, "/", length(co2files), " ", fn)
    #    cat(f, "/", length(co2files), " ", fn, "\n", file = "~/Desktop/log.txt", append = T)

    # Find and parse the header; its location varies by file
    top <- readLines(fn, n = 50)
    hl <- grep("TIMESTAMP", top)
    stopifnot(length(hl) == 1)
    hdr <- gsub('\\\"', "", top[hl])
    hdr <- strsplit(hdr, ",")[[1]]

    dat <- read.table(fn,
                      skip = hl + 1,  # skip units line after the header
                      sep = ",",
                      header = FALSE,
                      col.names = hdr,
                      na.strings = "NAN",
                      stringsAsFactors = FALSE)

    dat$TS <- as.POSIXct(dat$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
    dat <- dat[!is.na(dat$CO2) & !is.na(dat$TS),]
    dat <- dat[dat$Chamber > 0,]

    # Find new-chamber rows
    newchamber <- which(head(dat$Chamber, -1) != tail(dat$Chamber, -1)) + 1
    newchamber <- c(1, newchamber, nrow(dat) + 1)

    # This is an expensive step; create one d.f. per file
    resultsdf <- tibble(TIMESTAMP = rep(NA_character_, length(newchamber) - 1),
                        RECORD = NA_integer_,
                        Chamber = NA_integer_,
                        CO2 = NA_real_,
                        Tsoil = NA_real_,
                        Tair = NA_real_,
                        N = NA_integer_,
                        R2 = NA_real_,
                        Humity = NA_real_,
                        Flux = NA_real_,
                        Error = FALSE)

    for(i in seq_along(tail(newchamber, -1))) {
      d <- dat[newchamber[i]:(newchamber[i+1] - 1),]

      # Metadata says to use Equation 2 from https://www.nature.com/articles/sdata201726
      # Rs = 60.14 * Pair / (Tair + 273.15) * deltaC / deltaT
      d$secs <- d$TS - d$TS[1]
      m <- try(lm(CO2 ~ secs, data = d), silent = TRUE)
      mean_tair <- mean(d$Tair)
      if(class(m) == "lm") {
        resultsdf$Flux[i] <- 60.14 * 99.79 / (mean_tair + 273.15) * m$coefficients["secs"]
        resultsdf$R2[i] <- summary(m)$r.squared
      } else {
        resultsdf$Error[i] <- TRUE
      }

      if("Humity" %in% names(d)) {
        resultsdf$Humity[i] <- mean(d$Humity)
      }
      if("RECORD" %in% names(d)) {
        resultsdf$RECORD[i] <- d$RECORD[1]
      }

      resultsdf$TIMESTAMP[i] <- d$TIMESTAMP[1]
      resultsdf$Chamber[i] <- d$Chamber[1]
      resultsdf$CO2[i] <- mean(d$CO2)
      resultsdf$Tsoil[i] <- mean(d$Tsoil)
      resultsdf$Tair[i] <- mean_tair
      resultsdf$N[i] <- nrow(d)
    }
    results[[f]] <- resultsdf
  }
  rbind_list(results)
}

#' Parse one of the d20190617_SCOTT custom files
#'
#' @param path Data directory path, character
#' @param skip Lines to skip, integer
#' @param path Ports, an integer vector
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @keywords internal
parse_d20190617_SCOTT_xxx <- function(path, skip, ports) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  # File has header lines need to skip, and is in wide format
  # Build the column names
  cnames <- c("Year", "DOY")
  for(p in ports) {
    cnames <- c(cnames, paste(c("SR", "SM", "T5"), p, sep = "_"))
  }
  dat <- do.call("rbind",
                 lapply(files, read.table,
                        sep = ",",
                        header = FALSE,
                        col.names = cnames,
                        skip = skip,
                        na.strings = c("NaN"),
                        stringsAsFactors = FALSE))

  # Change the DOY column from fractional day of year to a "DOY time" string
  dat$DOY <- fractional_doy(dat$Year, dat$DOY)

  # Convert from wide to long format
  out <- data.frame()
  d <- dat[c("Year", "DOY")]
  for(p in ports) {
    dp <- d
    dp$CSR_PORT <- p
    dp$CSR_FLUX <- dat[,paste0("SR_", p)]
    dp$CSR_SM5 <- dat[,paste0("SM_", p)]
    dp$CSR_T5 <- dat[,paste0("T5_", p)]
    out <- rbind(out, dp)
  }

  out$CSR_ERROR <- FALSE
  out
}

#' Parse a custom file from d20190617_SCOTT_SRM
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
parse_d20190617_SCOTT_SRM <- function(path) {
  parse_d20190617_SCOTT_xxx(path, skip = 10, ports = 1:3)
}

#' Parse a custom file from d20190617_SCOTT_WKG
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
parse_d20190617_SCOTT_WKG <- function(path) {
  parse_d20190617_SCOTT_xxx(path, skip = 10, ports = 1:7)
}

#' Parse a custom file from d20190617_SCOTT_WHS
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
parse_d20190617_SCOTT_WHS <- function(path) {
  parse_d20190617_SCOTT_xxx(path, skip = 10, ports = 1:8)
}

#' Parse a custom file from d20190527_GOULDEN
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @keywords internal
parse_d20190527_GOULDEN <- function(path) {
  files <- list.files(path, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  # File has header lines, and records time as fractional days since noon on 2001-01-01
  dat <- do.call("rbind", lapply(files, read.csv,
                                 skip = 14,
                                 na.strings = c("NA", "-999"),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE))
  dat$Timestamp <- as.POSIXct("2001-01-01 12:00", format = "%Y-%m-%d %H:%M")  + dat$Day_of_study * 24 * 60 * 60
  dat$Timestamp <- as.character(dat$Timestamp)
  dat$CSR_ERROR <- FALSE
  dat[dat$Vegetation == "forest",]
}


#' Parse a custom file from d20190504_SAVAGE_hf006-05
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @keywords internal
`parse_d20190504_SAVAGE_hf006-05` <- function(path) {
  dat <- parse_PROCESSED_CSV(path)

  dat_control <- dat[dat$treatment == "C",]
  dat_control$CSR_T10 <- dat_control$soilt.c
  dat_control$CSR_SM10 <- dat_control$vsm.c
  dat_trench <- dat[dat$treatment != "C",]
  dat_trench$CSR_T10 <- dat_trench$soilt.t
  dat_trench$CSR_SM10 <- dat_trench$vsm.t

  rbind_list(list(dat_control, dat_trench))
}


#' Parse a custom file from d20190504_SAVAGE_hf006-03.
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @keywords internal
`parse_d20190504_SAVAGE_hf006-03` <- function(path) {
  dat <- parse_PROCESSED_CSV(path)

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


#' Parse a custom eofFD (forced diffusion) file from d20190430_DESAI
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.csv
#' @keywords internal
parse_d20190430_DESAI <- function(path) {
  dat <- parse_PROCESSED_CSV(path)

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

#' Parse a custom file from d20200109_HIRANO_PDB.
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
`parse_d20200109_HIRANO_PDB` <- function(path) {
  dat <- parse_PROCESSED_CSV(path)

  dat <- subset(dat, !(dat$DOY == 366 & dat$Time == 2400)) # drops one oddball row with no flux
  dat$DOYfrac <- fractional_doy(dat$Year, dat$DOY + dat$Time / 2400)

  # Flux fields
  fluxcols <- grep("^SR", names(dat))
  x <- dat[-fluxcols]
  results <- list()
  for(i in seq_along(fluxcols)) {
    results[[i]] <- x
    results[[i]]$CSR_FLUX <- dat[,fluxcols[i]]
    results[[i]]$CSR_PORT <- i
  }

  rbind_list(results)
}

#' Parse a custom file from d20200109_HIRANO_PDF.
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
`parse_d20200109_HIRANO_PDF` <- function(path) {
  parse_d20200109_HIRANO_PDB(path)
}

#' Parse a custom file from d20200108_JASSAL.
#'
#' @param path Data directory path, character
#' @return A \code{data.frame} containing extracted data.
#' @keywords internal
`parse_d20200122_BLACK` <- function(path) {
  dat <- parse_PROCESSED(path)

  dat$Timestamp <- as.character(dat$days_since_20040101 * 24 * 60 * 60 +
                                  strptime("20040101", format("%Y%m%d")))

  # Flux fields
  fluxcols <- grep("^flux_", names(dat))
  x <- dat[-fluxcols]
  results <- list()
  port_seq <- c(2, 3, 4, 5, 6, 9, 10, 11, 12) # custom sequence
  for(i in seq_along(fluxcols)) {
    results[[i]] <- x
    results[[i]]$flux <- dat[,fluxcols[i]]
    results[[i]]$port <- port_seq[i]
  }

  rbind_list(results)
}
