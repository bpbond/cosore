# test-processing.R

context("processing")

test_that("dataset read and report", {
  x <- read_dataset("TEST_licordata", raw_data = "TEST_licordata/")
  expect_is(x, "list")

  run_single_report(x, output_dir = tempdir(), quiet = TRUE)
})
