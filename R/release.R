# Copied from the baad repository...

make_cosore_release <- function(all_data, dest) {
  path <- file.path(tempfile(), "cosore_data")
  path <- "~/Desktop/temp/"
  dir.create(path, showWarnings = TRUE, recursive = TRUE)

  # saveRDS the object
  message("Saving database...")
  saveRDS(all_data, file = file.path(path, "cosore_data.RDS"))

  # invert structure and write each table as a csv

  # copy column metadata file
  message("Saving metadata...")
  f <- "CSR_COLUMNS_UNITS.txt"
  md <- system.file(file.path("extdata", f),
                    package = "cosore", mustWork = TRUE)
  file.copy(md, file.path(path, f))

  # run combined_report and copy it there

  #colophon(path)
  #  remake:::zip_dir(path, dest)
  message("Zipping...")
  utils::zip(file.path(path, "cosore.zip"),
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


colophon <- function(path) {
  git_sha <- system("git rev-parse HEAD", intern = TRUE)
  git_url <- paste0("https://github.com/dfalster/cosore/commit/", git_sha)
  file <- "colophon.Rmd"
  str <-
    c("# cosore: a Biomass And Allometry Database for woody plants",
      "",
      sprintf("**Release 1.0.1** git SHA: [%s](%s)", git_sha, git_url),
      "",
      "Session info used to generate this version:",
      "",
      "```{r}",
      "devtools::session_info()",
      "```")
  ## Working directories are a bit of a disaster zone in knitr, so
  ## we'll work around it here:
  owd <- setwd(path)
  on.exit(setwd(owd))
  writeLines(str, file)
  knitr::knit(file, quiet = TRUE)
  file.remove(file)
  invisible(file.path(path, file))
}
