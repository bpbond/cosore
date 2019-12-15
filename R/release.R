
#' Make a new COSORE release
#'
#' @param all_data A list of \code{cosore} datasets
#' @param path Path to write files to; must already exist
#' @param vignette_rebuilt Has vignette been rebuilt? Logical
#' @param force Ignore git dirty status? Logical
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
#' @note If a \code{CSR_EMBARGO} field exists in the a DESCRIPTION
#' file, no data will be released from that dataset.
#' @export
csr_make_release <- function(all_data, path,
                             vignette_rebuilt = FALSE, force = FALSE) {

  if(!force & length(system2("git", args = c("status", "--porcelain"), stdout = TRUE))) {
    stop("Not allowed: git working directory is not clean")
  }

  if(!dir.exists(path)) {
    stop("Path ", path, " doesn't exist")
  }

  if(!vignette_rebuilt) {
    stop("Vignette rebuilt via devtools::build_vignettes()?")
  }

  # Remove any datasets that are under embargo
  for(i in seq_along(all_data)) {
    if(!is.na(all_data[[i]]$description$CSR_EMBARGO)) {
      message(x$description$CSR_DATASET, " has an embargo entry--removing data")
      all_data[[i]]$data <- NULL
    }
  }

  # saveRDS the object
  message("Saving database...")
  saveRDS(all_data, file = file.path(path, "cosore_data.RDS"))

  # Invert structure and write each table (except data) as a single csv
  nms <- c("description", "contributors", "ports", "columns", "ancillary", "diagnostics")
  for(nm in nms) {
    message("Extracting ", nm)
    x <- csr_table(all_data, nm) # extract table with name "n"
    fn <- paste0(nm, ".csv")
    message("Writing ", fn)
    write.csv(x, file.path(path, fn), row.names = FALSE)
  }

  # For data, we write each dataset separately into a data/ folder
  message("Writing data tables...")
  p <- file.path(path, "datasets")
  dir.create(p, showWarnings = FALSE)
  lapply(all_data, function(x) {
    if(is.data.frame(x$data)) {
      write.csv(x$data,
                file = file.path(p, paste0("data_", x$description$CSR_DATASET, ".csv")),
                row.names = FALSE)
    }
  })

  # Copy column metadata and README files
  file_descriptions <- c(
    "cosore_data.RDS" = "Entire database saved as an RDS file (see below)",
    "ancillary.csv" = "Ancillary data (LAI, etc.) table",
    "columns.csv" = "Column mapping data table",
    "contributors.csv" = "Information on data contributors",
    "description.csv" = "General information about sites, instruments, and **citation information**",
    "diagnostics.csv" = "Diagnostics on data processing: records removed, etc.",
    "ports.csv" = "Port-specific information: species, collar areas and depths, treatments",
    "datasets" = "A folder containing the various `data` tables for each dataset",
    "CSR_COLUMNS_UNITS.txt" = "Metadata for all database fields",
    "Report-all.html" = "A summary report on the various datasets",
    "cosore-data-example.html" = "A vignette showing how to load and work with the database",
    "README.md" = "This file."
  )

  filelist <- c("CSR_COLUMNS_UNITS.txt", "README.md")
  for(f in filelist) {
    f_path <- system.file(file.path("extdata", f),
                          package = "cosore", mustWork = TRUE)
    f_data <- readLines(f_path)

    # Substitute in current information
    f_data <- gsub("%VERSION", packageVersion("cosore"), f_data)
    f_data <- gsub("%DATE", Sys.Date(), f_data)
    git_sha <- system2("git", args = "rev-parse HEAD", stdout = TRUE)
    git_sha <- substr(git_sha, 1, 8)
    f_data <- gsub("%GIT_SHA", git_sha, f_data)
    f_data <- gsub("%DATABASE_SIZE", format(object.size(all_data), "Mb"), f_data)
    f_data <- gsub("%FILELIST", paste(
      paste0("* **", names(file_descriptions), "** -"),
      file_descriptions,
      collapse = "\n"
    ), f_data)

    message("Writing ", f, "...")
    writeLines(f_data, file.path(path, f))
  }

  # Copy vignette file
  file.copy("doc/cosore-data-example.html", to = path)

  # Run combined_report and copy it there
  run_combined_report(all_data, output_dir = path)

  # Check: all the files should be included in the file description list
  all_files <- list.files(path, recursive = FALSE)
  release_file <- paste0("cosore-", packageVersion("cosore"), ".zip")
  all_files <- setdiff(all_files, release_file)
  missing <- !all_files %in% names(file_descriptions)
  if(any(missing)) {
    stop("Missing description for files: ",
         paste(all_files[missing], collapse = ", "))
  }
  extra <- !names(file_descriptions) %in% all_files
  if(any(missing)) {
    stop("Missing file for descriptions: ",
         paste(all_files[extra], collapse = ", "))
  }

  # Almost done! Zip everything up into a single file
  message("Zipping...")
  wd <- getwd()
  setwd(path)
  utils::zip(release_file,
             list.files("./", full.names = TRUE, recursive = TRUE))
  setwd(wd)

  file.path(path, release_file)
}
