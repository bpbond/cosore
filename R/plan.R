


#' Title
#'
#' @param ... Dataset objects
#'
#' @return x
#' @importFrom drake drake_plan
combine_data <- function(...) {
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
      # read in datasets into individual targets
      data = target(read_dataset(ds, rd),
                    # each data object is triggered by any change in the dataset directory;
                    # requires https://github.com/ropensci/drake/pull/795 (v7.1)
                    trigger = trigger(condition = file_in(dsf)),
                    # map the datasets and their directories to the targets above
                    transform = map(ds = !!datasets, dsf = !!dataset_folders, rd = !!raw_data,
                                    .id = dsf)), #
      # ...and combine into a single big list
      all = target(combine_data(data), transform = combine(data)),
      trace = TRUE
    )
  }
  csr_plan
}
