# Copied from the baad repository...

#' Make a new COSORE release
#'
#' @param all_data A list of \code{cosore} datasets.
#' @param dest Dest
#' @return Nothing; run for its side effects.
#' @export
make_cosore_release <- function(all_data, dest) {
  path <- file.path(tempfile(), "cosore_data")
  path <- "~/Desktop/temp/"
  dir.create(path, showWarnings = TRUE, recursive = TRUE)

  # saveRDS the object
  message("Saving database...")
  saveRDS(all_data, file = file.path(path, "cosore_data.RDS"))
  #browser()

  # invert structure and write each table as a csv
  # data is really big so not writing it for now--just in RDS file above
  nms <- c("description", "contributors", "ports", "columns", "ancillary") #, "data" )

  for(nm in nms) {
    message("Extracting ", nm)
    x <- csr_table(all_data, nm) # extract table with name "n"
    fn <- paste0(nm, ".csv")
    message("Writing ", fn)
    write.csv(x, file.path(path, fn), row.names = FALSE)
  }

  # copy column metadata file
  message("Saving metadata...")
  f <- "CSR_COLUMNS_UNITS.txt"
  md <- system.file(file.path("extdata", f),
                    package = "cosore", mustWork = TRUE)
  file.copy(md, file.path(path, f))

  # run combined_report and copy it there
  combined_report(all_data, output_dir = path)

  message("Zipping...")
  release_file <- file.path(path, "cosore.zip")
  utils::zip(release_file,
             list.files(path, full.names = TRUE),
             flags = "-j")
}

## Core data:
mydata_info <- function(path) {
  datastorr::github_release_info("bpbond/cosore",
                                 filename = NULL,
                                 read = readRDS,
                                 path = path)
}


##' Maintainer-only function for releasing data.  This will look at
##' the version in the DESCRIPTION file and make a data release if the
##' GitHub repository contains the same version as we have locally.
##' Requires the \code{GITHUB_TOKEN} environment variable to be set.
##'
##' @title Make a data release.
##' @param ... Parameters passed through to \code{\link{github_release_create}}
##' @param path Path to the data (see \code{\link{cosore}}).
##' @export
mydata_release <- function(..., path = NULL) {
  datastorr::github_release_create(mydata_info(path), ...)
}
