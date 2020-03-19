
#' Make a new COSORE release based on current package datasets.
#'
#' @param path Path to write files to; must already exist
#' @param vignette_rebuilt Has vignette been rebuilt? Logical
#' @param force Ignore git dirty status? Logical
#' @param run_report Include \code{combined_report.Rmd} in release? For testing
#' @param zip_release Run /code{utils::zip()} on output files? For testing
#' @param datasets Datasets to include, character; used for testing
#' @param sys_call System call to use, function; testing only; normally
#' \code{\link{system2}}
#' @return Fully qualified name of zip file containing release.
#' @importFrom utils packageVersion write.csv object.size
#' @details To make a new release, the git working directory
#' must be clean (unless \code{force = TRUE}), a \code{path} must
#' be specified, and there should be file descriptors (in the code)
#' for all files that will be zipped into the release;
#' this information is inserted into the \code{README.md} file.
#' Dynamic information like the current date, git commit hash,
#' database version, and
#' database size are all copied into \code{README.md} as well.
#' @note If a \code{CSR_EMBARGO} field exists in the DESCRIPTION
#' file, no data will be released from that dataset.
#' @export
csr_make_release <- function(path, vignette_rebuilt = FALSE, force = FALSE,
                             run_report = TRUE, zip_release = TRUE,
                             datasets = list_datasets(),
                             sys_call = system2) {

  stopifnot(is.character(path))
  stopifnot(is.logical(vignette_rebuilt))
  stopifnot(is.logical(force))
  stopifnot(is.logical(run_report))
  stopifnot(is.logical(zip_release))
  stopifnot(is.character(datasets))
  stopifnot(is.function(sys_call))

  if(!dir.exists(path)) {
    stop("Path ", path, " doesn't exist")
  }

  if(!force & length(list.files(path)) != 0) {
    stop("Not allowed: release directory is not empty")
  }

  if(!force) {
    if(length(sys_call("git", args = c("status", "--porcelain"), stdout = TRUE))) {
      stop("Not allowed: git working directory is not clean")
    }
  }

  if(!vignette_rebuilt) {
    stop("Vignette rebuilt via devtools::build_vignettes()?")
  }

  db_size <- 0

  # Invert structure and write each table (except data) as a single csv
  nms <- c("description", "contributors", "ports", "columns", "ancillary", "diagnostics")
  for(nm in nms) {
    message("Extracting ", nm)
    x <- csr_table(nm, datasets = datasets) # extract table with name "n"
    db_size <- db_size + object.size(x)
    fn <- paste0(nm, ".csv")
    message("Writing ", fn)
    write.csv(x, file.path(path, fn), row.names = FALSE)
  }

  # Write datasets' data
  for(dataset_name in datasets) {
    ds <- csr_dataset(dataset_name)
    db_size <- write_dataset_data(dataset_name, ds, path, db_size)
  } # for ds

  # Copy column metadata and README files
  file_descriptions <- c(
    "ancillary.csv" = "Ancillary data (LAI, etc.) table",
    "columns.csv" = "Column mapping data table",
    "contributors.csv" = "Information on data contributors",
    "description.csv" = "General information about sites, instruments, and **citation information**",
    "diagnostics.csv" = "Diagnostics on data processing: records removed, etc.",
    "ports.csv" = "Port-specific information: species, collar areas and depths, treatments",
    "datasets" = "A folder containing the various `data` tables for each dataset",
    "CSR_COLUMN_UNITS.csv" = "Metadata for all database fields",
    "LICENSE" = "CC-BY-4 license governing data use",
    "Report-all.html" = "A summary report on the entire database",
    "cosore-data-example.html" = "A vignette showing how to load and work with the database",
    "README.md" = "This file."
  )

  filelist <- c("CSR_COLUMN_UNITS.csv", "README.md")
  for(f in filelist) {
    f_path <- system.file(file.path("extdata", f),
                          package = "cosore", mustWork = TRUE)
    f_data <- readLines(f_path)
    f_data <- substitute_release_info(f_data, file_descriptions, db_size, force)

    message("Writing ", f, "...")
    writeLines(f_data, file.path(path, f))
  }

  # Copy license file
  file.copy(system.file("LICENSE", package = "cosore", mustWork = TRUE),
            to = path)

  # Copy vignette file
  if(file.exists("doc/cosore-data-example.html")) {
    file.copy("doc/cosore-data-example.html", to = path)
  } else { # write a blank file
    file.create(file.path(path, "cosore-data-example.html"))
  }

  # Run combined_report and copy it there
  if(run_report) {
    csr_report_database(output_dir = path)
  } else {
    # This normally happens only during testing; create a placeholder file
    file.create(file.path(path, "Report-all.html"))
  }

  # Check: all the files should be included in the file description list
  all_files <- list.files(path, recursive = FALSE)
  release_file <- paste0("cosore-", packageVersion("cosore"), ".zip")
  all_files <- setdiff(all_files, release_file)
  check_file_description_consistency(all_files, file_descriptions)

  # Almost done! Zip everything up into a single file
  if(zip_release) {
    message("Zipping...")
    wd <- getwd()
    setwd(path)
    utils::zip(release_file,
               list.files("./", full.names = TRUE, recursive = TRUE))
    setwd(wd)
  }

  file.path(path, release_file)
}

#' Substitute Git SHA, date, etc., into file data.
#'
#' @param f_data File data, a character vector
#' @param file_descriptions A character vector of filenames and their descriptions
#' @param db_size Database size, numeric
#' @param force Ignore git dirty status? Logical
#' @return The file data with substitutions made.
#' @note Called only by \code{\link{csr_make_release}}.
#' @keywords internal
substitute_release_info <- function(f_data, file_descriptions, db_size, force) {
  stopifnot(is.character(f_data))
  stopifnot(is.character(file_descriptions))
  stopifnot(is.numeric(db_size))

  # Substitute in current information
  f_data <- gsub("%VERSION", packageVersion("cosore"), f_data)
  f_data <- gsub("%DATE", Sys.Date(), f_data)
  git_sha <- system2("git", args = "rev-parse HEAD", stdout = TRUE)
  git_sha <- substr(git_sha, 1, 8)
  if(!force) {
    f_data <- gsub("%GIT_SHA", git_sha, f_data)
  } else {
    f_data <- gsub("%GIT_SHA", paste(git_sha, "- BUT GIT DIRECTORY WAS NOT CLEAN ON RELEASE"), f_data)
  }
  f_data <- gsub("%DATABASE_SIZE", format(db_size, units = "Mb"), f_data)
  gsub("%FILELIST", paste(
    paste0("* **", names(file_descriptions), "** -"),
    file_descriptions,
    collapse = "\n"
  ), f_data)
}

#' Substitute Git SHA, date, etc., into file data.
#'
#' @param dataset_name Dataset name, character
#' @param ds Dataset, a list
#' @param path Output path, character
#' @param db_size Database size, numeric
#' @return Updated database size.
#' @note Called only by \code{\link{csr_make_release}}.
#' @keywords internal
write_dataset_data <- function(dataset_name, ds, path, db_size) {
  stopifnot(is.character(dataset_name))
  stopifnot(is.list(ds))
  stopifnot(is.character(path))
  stopifnot(is.numeric(db_size))

  # If dataset is under embargo, don't write data
  if(!is.null(ds$description$CSR_EMBARGO) &&
     !is.na(ds$description$CSR_EMBARGO)) {
    message(dataset_name, " has an embargo entry--skipping data")
    return(db_size)
  }

  # For data, we write each dataset separately into a data/ folder
  message("\tWriting data...")
  p <- file.path(path, "datasets")
  dir.create(p, showWarnings = FALSE)
  if(is.data.frame(ds$data)) {
    write.csv(ds$data,
              file = file.path(p, paste0("data_", dataset_name, ".csv")),
              row.names = FALSE)
  }
  db_size + object.size(ds$data)
}


#' Check that files have descriptions and descriptions have files.
#'
#' @param all_files Vector of filenames, characfter
#' @param file_descriptions Vector of file descriptions, character
#' @return Nothing; run for side effect of error if necessary.
#' @keywords internal
#' @note Called only by \code{\link{csr_make_release}}.
check_file_description_consistency <- function(all_files, file_descriptions) {
  stopifnot(is.character(all_files))
  stopifnot(is.character(file_descriptions))

  missing <- !all_files %in% names(file_descriptions)
  if(any(missing)) {
    stop("Missing description for files: ",
         paste(all_files[missing], collapse = ", "))
  }
  extra <- !names(file_descriptions) %in% all_files
  if(any(extra)) {
    stop("Missing file for descriptions: ",
         paste(names(file_descriptions)[extra], collapse = ", "))
  }

}
