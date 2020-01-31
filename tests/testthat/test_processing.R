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
  expect_warning(x_no_data <- read_dataset("TEST_licordata"))
  csr_report_dataset(x_no_data, output_dir = tempdir(), quiet = TRUE)
})
