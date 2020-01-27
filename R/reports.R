
#' Run a summary report on the entire COSORE database
#'
#' @param output_dir Output directory
#' @importFrom rmarkdown render
#' @return Nothing; run for its side effects.
#' @export
csr_report_database <- function(output_dir = "~/Desktop/Reports/") {
  mf <- system.file("reports/combined_report.Rmd", package = "cosore")

  rmarkdown::render(mf,
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
#' @param quick Passed on to \code{rmarkdown::render}
#' @importFrom rmarkdown render
#' @return Nothing; run for its side effects.
#' @export
csr_report_dataset <- function(ds,
                               output_dir = "~/Desktop/Reports/",
                               quiet = FALSE, quick = FALSE) {

  mf <- system.file("reports/dataset_report.Rmd", package = "cosore")

  rmarkdown::render(mf,
                    params = list(dataset = ds, quick = FALSE, quick = quick),
                    output_file = paste0("Report-", ds$description$CSR_DATASET, ".html"),
                    output_dir = output_dir,
                    quiet = quiet)
}

#' Run all individual dataset and database reports
#'
#' @return Nothing; run for its side effects.
#' @export
csr_reports_all <- function() {
  for(dataset_name in list_datasets()) {
    csr_report_dataset(csr_dataset(dataset_name))
  }

    csr_report_database(all_data)
}
