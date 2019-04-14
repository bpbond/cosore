

run_reports <- function(all_data) {
  # individual dataset reports
  mf <- system.file("reports/datareport.Rmd", package = "cosore")

  for(dsn in names(all_data)) {
    rmarkdown::render(mf,
                      params = list(all = all, dataset_name = dsn),
                      output_file = paste0("Report-", dsn, ".html"),
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
#' csr_build("")  # build without raw data
#' \dontrun{
#' csr_build("../rawdata/")  # build without raw data
#' }
csr_build <- function(raw_data) {
  datasets <- list_datasets()

  if(length(datasets)) { # if no data, don't build
    dataset_folders <- resolve_dataset(datasets)

    csr_plan <- drake_plan(
      datasets = list_datasets(),
#      dss = rlang::syms(datasets),

      # read in datasets into individual targets
      data = target(read_dataset(ds, rd),
                    # each data object is triggered by any change in the dataset directory;
                    # requires https://github.com/ropensci/drake/pull/795 (v7.1)
                    trigger = trigger(condition = file_in(dsf)),
                    # map the datasets and their directories to the targets above
                    transform = map(ds = !!datasets, dsf = !!dataset_folders,
                                    rd = !!raw_data, .id = ds)), #
      # ...and combine into a single big list
      all = target(combine_data(datasets, data), transform = combine(data)),

      trace = TRUE
    )
  }
  csr_plan
}
