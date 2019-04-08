


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

datasets <- list_datasets()

if(length(datasets)) { # if no data, don't build
  cosore_plan <- drake_plan(
    data = target(read_dataset(ds), transform = map(ds = !!datasets)),
    all = target(combine_data(data), transform = combine(data)),
    trace = TRUE
  )
}
