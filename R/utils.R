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
#' @export
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
#' This is typically used to write standardized data
#' to \code{inst/extdata/datasets}. These data can be removed
#' by \code{\link{csr_remove_stan_data}}.
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
    dataset_name <- x$description$CSR_DATASET
    message(dataset_name)
    outpath <- file.path(path, dataset_name, "data")
    if(!dir.exists(outpath)) {
      if(create_dirs) {
        message("Creating ", outpath)
        dir.create(outpath, recursive = TRUE)
      } else {
        stop(outpath, " does not exist")
      }
    }

    datafiles <- character(0)
    if(is.data.frame(x$data)) {
      # Write respiration data
      # csv (big, version control friendly) or RDS (small, fast, preserves types)?
      # Going with the latter for now
      outfile <- file.path(outpath, paste0("data_", dataset_name, ".RDS"))
      saveRDS(x$data, file = outfile)
      # Write diagnostics data
      diagfile <- file.path(outpath, paste0("diag_", dataset_name, ".RDS"))
      saveRDS(x$diagnostics, file = diagfile)
    }
  })
  invisible(NULL)
}

#' Remove standardized dataset(s)
#'
#' Utility function that remove standardized datasets created
#' by \code{\link{csr_standardize_data}} from a
#' directory tree, specifically \code{{path}/dataset/data/*.RDS}.
#'
#' @param path Path, typically \code{inst/extdata/datasets/}
#' @param datasets Optional character vector of datasets. If not
#' supplied, the output of \code{\link{list_datasets}} is used.
#' @return Nothing.
#' @export
csr_remove_stan_data <- function(path, datasets = list_datasets()) {
  for(ds in datasets) {
    message(ds)
    data_dir <- file.path(path, ds, "data")
    if(!dir.exists(data_dir)) {
      message("\tNo data directory")
      next
    }
    files <- list.files(data_dir, pattern = "*.RDS", full.names = TRUE)
    message("\tRemoving ", length(files), " file(s)")
    unlink(files)
  }
}


#' Convert a vector of strings to POSIXct timestamps.
#'
#' @param ts Timestamps, character
#' @param timestamp_format Formatting string that \code{\link{as.POSIXct}} accepts.
#' This normally comes from \code{CSR_TIMESTAMP_FORMAT} in the DESCRIPTION file.
#' @param time_zone Time zone that \code{\link{as.POSIXct}} accepts.
#' This normally comes from \code{CSR_TIMESTAMP_TZ} in the DESCRIPTION file.
#' @return A list with (i) a vector of \code{POSIXct} timestamps, (ii) a logical vector
#' indicating which ones are invalid (and thus now NA), and (iii) a string of examples of
#' bad timestamps.
#' @keywords internal
convert_and_qc_timestamp <- function(ts, timestamp_format, time_zone) {
  stopifnot(is.character(ts))
  stopifnot(is.character(timestamp_format))
  stopifnot(is.character(time_zone))

  new_ts <- as.POSIXct(ts, format = timestamp_format, tz = time_zone)
  na_ts <- is.na(new_ts) & !is.na(ts)
  bad_examples <- paste(head(ts[na_ts]), collapse = ", ")
  list(new_ts = new_ts, na_ts = na_ts, bad_examples = bad_examples)
}

#' Combine all datasets into a single list
#'
#' @param datasets Character vector of dataset names
#' @param ... Dataset objects
#' @return A single list with all datasets
#' @importFrom drake drake_plan
#' @keywords internal
combine_data <- function(datasets, ...) {
  x <-  list(...)
  names(x) <- datasets
  x
}


#' Compute measurement interval for a dataset
#'
#' @param dsd Dataset data (a data frame)
#' @return Median interval between timestamps.
#' @importFrom stats median
#' @note This is used by the reports.
compute_interval <- function(dsd) {
  stopifnot(is.data.frame(dsd))

  dsd <- dsd[with(dsd, order(CSR_TIMESTAMP_BEGIN, CSR_PORT)),]
  dsd$Year <- lubridate::year(dsd$CSR_TIMESTAMP_BEGIN)
  mylag <- function(x) c(as.POSIXct(NA), head(x, -1))  # like dplyr::lag()

  results <- list()
  for(y in unique(dsd$Year)) {
    for(p in unique(dsd$CSR_PORT)) {
      d <- dsd[dsd$Year == y & dsd$CSR_PORT == p,]

      results[[paste(y, p)]] <-
        tibble(Year = y,
               Port = p,
               N = nrow(d),
               Interval = median(as.numeric(difftime(d$CSR_TIMESTAMP_BEGIN,
                                                            mylag(d$CSR_TIMESTAMP_BEGIN),
                                                            units = "mins")), na.rm = TRUE))
    }
  }

  cosore::rbind_list(results)
}

