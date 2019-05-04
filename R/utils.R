# utils.R

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
#' @param path Path to search; normally the metadata are in \code{inst/extdata}
#' @param write_files Write resulting files back out? Logical
#' @return Nothing; run for side effects
#' @export
insert_line <- function(file, pattern, newlines, after = TRUE, path = "./inst/extdata", write_files = TRUE) {
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
}
