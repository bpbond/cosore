
#' Run a summary report on the entire COSORE database
#'
#' @param all_data A list of \code{cosore} datasets.
#' @param output_dir Output directory
#' @importFrom rmarkdown render
#' @return Nothing; run for its side effects.
#' @export
run_combined_report <- function(all_data, output_dir = "~/Desktop/Reports/") {
  mf <- system.file("reports/combined_report.Rmd", package = "cosore")

  rmarkdown::render(mf,
                    params = list(all_data = all_data),
                    output_file = paste0("Report-all.html"),
                    output_dir = output_dir)
}


#' Run a single dataset report
#'
#' Run a single dataset report, writing the resulting \code{html}
#' file into a specified directory.
#'
#' @param ds The dataset for which to generate a report.
#' @param output_dir Output directory, character
#' @param quiet Passed on to \code{rmarkdown::render}
#' @importFrom rmarkdown render
#' @return Nothing; run for its side effects.
#' @export
run_single_report <- function(ds, output_dir = "~/Desktop/Reports/", quiet = FALSE) {
  mf <- system.file("reports/dataset_report.Rmd", package = "cosore")

  rmarkdown::render(mf,
                    params = list(dataset = ds, quick = FALSE),
                    output_file = paste0("Report-", ds$description$CSR_DATASET, ".html"),
                    output_dir = output_dir,
                    quiet = quiet)
}

#' Run all individual dataset reports
#'
#' @param all_data A list of \code{cosore} datasets.
#' @param run_combined When done, run the combined (entire database) report?
#' @return Nothing; run for its side effects.
#' @export
run_all_reports <- function(all_data, run_combined = TRUE) {
  for(ds in all_data) {
    run_single_report(ds)
  }

  if(run_combined) {
    run_combined_report(all_data)
  }
}
