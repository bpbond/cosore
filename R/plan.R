
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

#' Build the cosore dataset
#'
#' @param raw_data The raw data folder to use, character path
#' @param return_all_data Return all data generated (invisibly)?, Logical
#' @return All the built data, invisibly
#' @importFrom drake drake_plan
#' @export
#' @examples
#' csr_build("")  # build without raw data; metadata only
#' \dontrun{
#' csr_build("../rawdata/")  # build with raw data
#' }
csr_build <- function(raw_data, return_all_data = TRUE) {
  # silence package check notes
  combine <- dat <- dsf <- dsn <- file_in <- map <- target <- trigger <- NULL

  dataset_names <- list_datasets()
  dataset_folders <- resolve_dataset(dataset_names)

  if(length(dataset_names)) { # if no data, don't build
    csr_plan <- drake_plan(

      # read in datasets into individual targets
      dat = target(read_dataset(dsn, raw_data),
                   # each data object is triggered by any change in the dataset directory;
                   # requires https://github.com/ropensci/drake/pull/795 (v7.1)
                   trigger = trigger(condition = file_in(dsf)),
                   # map the datasets and their directories to the targets above
                   transform = map(dsn = !!dataset_names, dsf = !!dataset_folders, .id = dsn)),
      # ...and combine into a single big list
      all = target(combine_data(dataset_names, dat), transform = combine(dat)),

      trace = TRUE
    )
    drake::make(csr_plan)

    if(return_all_data) {
      invisible(drake::readd("all"))
    }
  }
}

# csr_build("../rawdata/")
# all <- drake::readd("all")
# run_reports(all)
# run_combined_report(all)

