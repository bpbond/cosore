
#' Make a new COSORE release
#'
#' @param all_data A list of \code{cosore} datasets
#' @param path Path to write files to; must already exist
#' @param force Ignore git dirty status? Logical
#' @return Fully qualified name of zip file containg release.
#' @importFrom utils packageVersion write.csv object.size
#' @details To make a new release, the git working directory
#' must be clean (unless \code{force = TRUE}), a \code{path} must
#' be specified, and there should be file descriptors (in the code)
#' for all files that will be zipped into the release;
#' this information is inserted into the \code{README.md} file.
#' Dynamic information like the current date, git commit hash,
#' database version, and
#' database size are all copied into \code{README.md} as well.
#' @export
make_cosore_release <- function(all_data, path, force = FALSE) {

  if(!force & length(system2("git", args = c("status", "--porcelain"), stdout = TRUE))) {
    stop("Not allowed: git working directory is not clean")
  }

  if(!dir.exists(path)) {
    stop("Path ", path, " doesn't exist")
  }

  # saveRDS the object
  message("Saving database...")
  saveRDS(all_data, file = file.path(path, "cosore_data.RDS"))

  # Invert structure and write each table (except data) as a single csv
  nms <- c("description", "contributors", "ports", "columns", "ancillary")
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
                file = file.path(p, paste0("data_", x$dataset_name, ".csv")),
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
    "ports.csv" = "Port-specific information: species, collar areas and depths, treatments",
    "datasets" = "A folder containing the various `data` tables for each dataset",
    "CSR_COLUMNS_UNITS.txt" = "Metadata for all database fields",
    "Report-all.html" = "A summary report on the various datasets",
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

  # Run combined_report and copy it there
  combined_report(all_data, output_dir = path)

  # Check: all the files should be included in the file description list
  all_files <- list.files(path, recursive = FALSE)
  missing <- !all_files %in% names(file_descriptions)
  if(any(missing)) {
    stop("Missing file descriptions for: ",
         paste(all_files[missing], collapse = ", "))
  }

  # Almost done! Zip everything up into a single file
  message("Zipping...")
  release_file <- paste0("cosore-", packageVersion("cosore"), ".zip")
  wd <- getwd()
  setwd(path)
  utils::zip(release_file,
             list.files("./", full.names = TRUE, recursive = TRUE))
  setwd(wd)

  file.path(path, release_file)
}
