

#' Run reports
#'
#' @param all_data A list of \code{cosore} datasets.
#' @importFrom rmarkdown render
#' @return Nothing; run for its side effects.
#' @export
run_reports <- function(all_data) {
  # individual dataset reports
  mf <- system.file("reports/dataset_report.Rmd", package = "cosore")

  for(ds in all_data) {
    rmarkdown::render(mf,
                      params = list(dataset = ds),
                      output_file = paste0("Report-", ds$dataset_name, ".html"),
                      output_dir = "~/Desktop/Reports/")
  }
}

#' Title
#'
#' @param datasets Character vector of dataset names
#' @param ... Dataset objects
#' @return x
#' @importFrom drake drake_plan
#' @export
combine_data <- function(datasets, ...) {
  x <-  list(...)
  names(x) <- datasets
  x
}

#' Build the cosore dataset.
#'
#' @param raw_data The raw data folder to use, character path
#' @return The drake plan.
#' @importFrom drake drake_plan
#' @export
#' @examples
#' csr_build("")  # build without raw data; metadata only
#' \dontrun{
#' csr_build("../rawdata/")  # build with raw data
#' }
csr_build <- function(raw_data) {
  dataset_names <- list_datasets()
  dataset_folders <- resolve_dataset(dataset_names)

  if(length(dataset_names)) { # if no data, don't build
    csr_plan <- drake_plan(
      #      dss = rlang::syms(datasets),

      # read in datasets into individual targets
      dat = target(read_dataset(dsn, rd),
                   # each data object is triggered by any change in the dataset directory;
                   # requires https://github.com/ropensci/drake/pull/795 (v7.1)
                   trigger = trigger(condition = file_in(dsf)),
                   # map the datasets and their directories to the targets above
                   transform = map(dsn = !!dataset_names, dsf = !!dataset_folders,
                                   rd = !!raw_data, .id = dsn)), #
      # ...and combine into a single big list
      all = target(combine_data(dataset_names, dat), transform = combine(dat)),

      trace = TRUE
    )
    drake::make(csr_plan)
  }
}
