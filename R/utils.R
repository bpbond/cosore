# utils.R


#' fractional_doy
#'
#' Convert a day of year (DOY) given as xxx.xxx to a day of year + time of day.
#' This can vary due to leap years, so we need the year in question as well.
#'
#' @param year Year, integer
#' @param doy Day of year, numeric (xxx.xxx)
#' @return A string giving day of year and time in \code{\%j \%T} format; see \code{\link{strptime}}.
#' @keywords internal
fractional_doy <- function(year, doy) {
  stopifnot(all(doy >= 1, na.rm = TRUE))
  stopifnot(all(doy < 367, na.rm = TRUE))

  doy_floor <- floor(doy)
  start <- strptime(paste(year, doy_floor, "00:00:00"), "%Y %j %T")
  frac <- doy - doy_floor
  end <- start + frac * 24 * 60 * 60
  format(end, "%j %T")
}


rename_col <- function(x, old, new) {
  stopifnot(old %in% names(x))
  colnames(x)[colnames(x) == old] <- new
  x
}


#' Split a raw Licor (*.81x) file into pieces.
#'
#' @param filename Fully-qualified filename, character
#' @param split_lines Rough number of lines to split by, integer
#' @param out_dir Output directory, character
#' @note Licor output files can get \emph{very} big. This utility splits (on record
#' boundaries) a file into multiple sub-files.
#' @return Number of files created, invisibly.
#' @keywords internal
split_licor_file <- function(filename, split_lines = 25000, out_dir = dirname(filename)) {

  stopifnot(split_lines > 0)
  stopifnot(file.exists(filename))
  stopifnot(dir.exists(out_dir))

  # Read file into memory and find records
  filedata <- readLines(filename)
  record_starts <- grep(pattern = "^LI-8100", filedata)
  message("Reading ", filename, ": lines = ", length(filedata), " records = ", length(record_starts))
  bfn <- gsub(".81x$", "", basename(filename))  # remove extension

  this_file_start_record <- 1
  startline <- 1
  filenum <- 0
  while(this_file_start_record <= length(record_starts)) {
    filenum <- filenum + 1
    within_file <- record_starts - record_starts[this_file_start_record] < split_lines
    within_file[this_file_start_record] <- TRUE # always write at least one record
    next_file_start_record <- max(which(within_file)) + 1

    if(next_file_start_record > length(record_starts)) {
      endline <- length(filedata)
    } else {
      endline <- record_starts[next_file_start_record] - 1
    }

    new_file <- file.path(out_dir, paste0(bfn, "_", sprintf("%06d", filenum), ".81x"))
    message("Writing lines", startline, "-", endline, "to ", new_file, "\n")
    cat(filedata[startline:endline], sep = "\n", file = new_file)
    this_file_start_record <- next_file_start_record
    startline <- endline + 1
  }
  invisible(filenum)
}

#' Insert new line(s) into existing metadata files.
#'
#' @param file Filename, character
#' @param pattern Pattern to search for, a regular expression
#' @param newlines New line(s) to write, character vector
#' @param after Insert new line(s) after pattern (T) or before (F)? Logical
#' @param path Path to search; normally the metadata are in \code{inst/extdata/datasets}
#' @param write_files Write resulting files back out? Logical
#' @return Nothing; run for side effects
#' @keywords internal
insert_line <- function(file, pattern, newlines, after = TRUE, path = "./inst/extdata/datasets", write_files = TRUE) {
  files <- list.files(path, pattern = file, full.names = TRUE, recursive = TRUE)
  message("Found ", length(files), " files")

  for(f in files) {
    dat <- readLines(f)
    pat <- grep(pattern, dat)
    if(after) {
      ip <- pat + 1
    } else {
      ip <- pat
    }

    if(length(ip) > 1) {
      stop("Found more than one possible insertion point in ", f)
    }
    if(length(ip)) {
      # Have we already been here?
      if(dat[ip] == newlines[1]) {
        warning("Lines to insert already found at line ", ip, " in ", f)
      } else {
        message("Found insert point at line ", ip, " in ", f)
        dat <- c(dat[1:(ip - 1)], newlines, dat[ip:length(dat)])
        if(write_files) {
          message("...writing")
          writeLines(dat, f)
        }
      }
    } else {
      warning("No insert point found in ", f)
    }
  }
}


#' Bind a list of data frames (with possibly non-identical column names) together.
#'
#' @param x List of data frames
#' @return A single \code{data.frame} with all data together
#' @keywords internal
rbind_list <- function(x) {
  stopifnot(is.list(x))

  all_names <- unique(unlist(lapply(x, function(x) names(x))))

  tibble::as_tibble(
    do.call(rbind,
            c(lapply(x, function(x_entry) {
              if(is.null(x_entry)) {
                data.frame()
              } else {
                data.frame(c(x_entry, sapply(setdiff(all_names, names(x_entry)),
                                             function(y) NA)),
                           check.names = FALSE, stringsAsFactors = FALSE)
              }
            }), make.row.names = FALSE, stringsAsFactors = FALSE))
  )
}


#' Write out standardized data and diagnostics tables
#'
#' This is typically used to write standardized data to \code{inst/extdata/datasets}.
#'
#' @param all_data A list of \code{cosore} datasets
#' @param path Output path, character
#' @param create_dirs Create subdirectories as needed? Logical
#' @return Nothing.
#' @export
csr_standardize_data <- function(all_data, path, create_dirs = FALSE) {

  stopifnot(is.list(all_data))
  stopifnot(is.character(path))
  stopifnot(is.logical(create_dirs))

  message("Writing data and diagnostic tables...")
  p <- file.path(path, "datasets")
  lapply(all_data, function(x) {
    if(is.data.frame(x$data)) {
      message(x$description$CSR_DATASET)
      outpath <- file.path(path, x$description$CSR_DATASET)
      if(!dir.exists(outpath)) {
        if(create_dirs) {
          message("Creating ", outpath)
          dir.create(outpath, recursive = TRUE)
        } else {
          stop(outpath, " does not exist")
        }
      }
      # Write diagnostics data
      diagfile <- file.path(outpath, paste0("diagnostics_", x$description$CSR_DATASET, ".csv"))
      write.csv(x$diagnostics, file = diagfile, row.names = FALSE)

      # Write respiration data
      # To limit file sizes we write years into individual files
      years <- unique(lubridate::year(x$data$CSR_TIMESTAMP))
      for(y in years) {
        message(" ", y, appendLF = FALSE)
        d <- x$data[lubridate::year(x$data$CSR_TIMESTAMP) == y,]
        datafile <- file.path(outpath, paste0("data_", x$description$CSR_DATASET, "_", y, ".csv"))
        write.csv(d, file = datafile, row.names = FALSE)
      }
      message()
    }
  })
  invisible(NULL)
}
