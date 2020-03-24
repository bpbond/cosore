# test-reports.R

context("dataset_read")

test_that("dataset read and report", {

  # Missing raw data folder
  expect_message(x <- read_dataset("TEST_licordata", raw_data = tempdir()),
                 regexp = "No data folder found")
  expect_is(x, "list")
  expect_null(x$data)

  # read_dataset
  x <- read_dataset("TEST_licordata", raw_data = "test_raw_data/")
  expect_is(x, "list")
  csr_report_dataset(x, output_dir = tempdir(), quiet = TRUE)

  expect_true(is.POSIXct(x$data$CSR_TIMESTAMP_BEGIN))
  expect_true(is.POSIXct(x$data$CSR_TIMESTAMP_END))
  expect_type(x$data$CSR_FLUX_CO2, "double")
  expect_true(is.numeric(x$data$CSR_PORT))

  # No-data dataset
  expect_message(x_no_data <- read_dataset("TEST_licordata"),
                 regexp = "No standardized or raw data found")
  csr_report_dataset(x_no_data, output_dir = tempdir(), quiet = TRUE)

  # Custom file format dataset
  expect_error(read_dataset("TEST_custom", raw_data = "test_raw_custom/"), regexp = "TEST_custom dispatched OK")
  expect_warning(read_dataset("TEST_custom_missing", raw_data = "test_raw_custom/"), regexp = "Unknown format")

  # Missing CrvFitStatus
  x <- parse_LI8100_file("test_raw_crvfitstatus/rp7_crvfitstatus.txt")
  expect_identical(x$CrvFitStatus, "Lin")

  # Zero records
  x <- parse_LI8100_file("test_raw_problems/rp6_empty_file.txt")
  expect_null(x)

  # Unusual error conditions
  for(fn in list.files(pattern = "*.txt", path = "test_raw_problems", full.names = TRUE)) {
    rec <- readLines(fn)
    x <- parse_LI8100_record(rec, data.frame(), 0, basename(fn), 0)
    # These errors should return data frame with `Error` column
    expect_true("Error" %in% names(x), info = fn)
  }
})
