# test-processing.R

context("processing")

test_that("dataset read and report", {
  x <- read_dataset("TEST_licordata", raw_data = "test_raw_data/")
  expect_is(x, "list")
  csr_report_dataset(x, output_dir = tempdir(), quiet = TRUE)

  expect_true(is.POSIXct(x$data$CSR_TIMESTAMP_BEGIN))
  expect_true(is.POSIXct(x$data$CSR_TIMESTAMP_END))
  expect_type(x$data$CSR_FLUX, "double")
  expect_true(is.numeric(x$data$CSR_PORT))

  # No-data dataset
  expect_message(x_no_data <- read_dataset("TEST_licordata"))
  csr_report_dataset(x_no_data, output_dir = tempdir(), quiet = TRUE)

  # Unusual error conditions
  for(fn in list.files(pattern = "*.txt", path = "test_raw_problems", full.names = TRUE)) {
    rec <- readLines(fn)
    x <- parse_LI8100_record(rec, data.frame(), 0, basename(fn), 0)
    # These errors should return data frame with `Error` column
    expect_true("Error" %in% names(x), info = fn)
  }
})
