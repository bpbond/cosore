# test_user.R

context("user")

test_that("csr_metadata", {
  # ¯\_(ツ)_/¯
  md <- csr_metadata()
  expect_true(tibble::is_tibble(md))
})

test_that("csr_table", {
  # Handles bad data
  expect_error(csr_table(), regexp = "should be one of")  # informative error message

  x <- csr_table("sdfkjh")  # should return an empty data frame
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 0L)
})

test_that("csr_dataset", {
  # Handles bad data
  expect_error(csr_dataset(1))
  expect_error(csr_dataset(LETTERS))
  expect_error(csr_dataset("x", quiet = "quiet"))
  expect_error(csr_dataset("x", metadata_only = "metadata_only"))

  x <- csr_dataset("TEST_licordata", metadata_only = TRUE)
  expect_is(x, "list")
  expect_identical(sort(names(x)),
                   sort(c("description", "contributors", "ports", "columns", "ancillary")))
})

test_that("csr_database", {
  expect_error(csr_database(1))

  # Make sure that the internal COSORE_SUMMARY object has been updated
  # NB this won't catch changes to the dataset, e.g. the number of data records

  x <- csr_database(regenerate = FALSE)
  expect_s3_class(x, "data.frame")
  expect_identical(sort(x$CSR_DATASET), sort(list_datasets()))
})

test_that("extract_table", {
  expect_s3_class(extract_table("TEST_licordata", "description", TRUE), "data.frame")
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) NULL))
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) list(x = 1)))
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) list(x = data.frame())))
})

test_that("csr_standardize_data", {
  # Handles bad input
  expect_error(csr_standardize_data(1, "", TRUE))
  expect_error(csr_standardize_data(list(), 1, TRUE))
  expect_error(csr_standardize_data(list(), "", 1))

  library(lubridate)
  data1 <- tibble(CSR_TIMESTAMP = ymd_hms(c("2020-01-01 12:34:56",
                                            "2021-01-01 12:34:56")))
  ds1 <- list(description = tibble(CSR_DATASET = "ds1"),
              diagnostics = tibble(),
              data = data1)
  td <- tempdir()

  # Error - data subdirectory doesn't exist
  expect_error(csr_standardize_data(ds1, td, create_dirs = FALSE),
               regexp = "does not exist")

  expect_null(csr_standardize_data(ds1, td, create_dirs = TRUE))
  td_dataset <- file.path(td, "ds1", "data")

  # Subdirectory was created
  expect_true(dir.exists(td_dataset))
  # Files exist
  expect_true(file.exists(file.path(td_dataset, "diag.RDS")))
  expect_true(file.exists(file.path(td_dataset, "data.RDS")))
  written_data <- readRDS(file.path(td_dataset, "data.RDS"))
  expect_identical(ds1$data, written_data)

  # Removing standardized files works
  csr_remove_stan_data(td, datasets = "ds1")
  expect_identical(list.files(td_dataset), character(0))
  expect_message(csr_remove_stan_data("directory_doesnt_exist", datasets = "ds1"))

  # Handles no-data dataset: should create directory but write no files
  ds2 <- list(description = tibble(CSR_DATASET = "ds2"))

  csr_standardize_data(ds2, td, create_dirs = TRUE)
  td_dataset <- file.path(td, "ds2", "data")
  expect_true(dir.exists(td_dataset))
  expect_identical(length(list.files(td_dataset)), 0L)

  csr_remove_stan_data(td, datasets = "ds2")
  expect_identical(list.files(td_dataset), character(0))
})
