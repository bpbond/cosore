# test-utils.R

context("utils")

test_that("rbind_list", {
  expect_equivalent(rbind_list(list(NULL)), data.frame())
  expect_equivalent(rbind_list(list(cars, NULL)), cars)
  expect_error(rbind_list(cars))

  x <- data.frame(a = 1)
  y <- data.frame(b = 2)
  z <- rbind_list(list(x, y))
  expect_is(z, "data.frame")
  expect_identical(nrow(z), nrow(x) + nrow(y))
  expect_identical(names(z), union(names(x), names(y)))
  expect_equivalent(rbind_list(list(x, x)), rbind(x, x))
})

test_that("split_licor_file", {
  tf <- tempfile()
  nr <- "LI-8100"  # new record delimiter

  # handles empty file
  cat("", sep = "\n", file = tf)
  expect_equal(split_licor_file(tf), 0)

  cat(rep(c(nr, rep("data", 10)), 2), sep = "\n", file = tf)

  # split_lines smaller than record size
  expect_equal(split_licor_file(tf, split_lines = 2), 2)
  # split_lines larger than record size
  expect_equal(split_licor_file(tf, split_lines = 20), 1)
  # split_lines larger than file size
  expect_equal(split_licor_file(tf, split_lines = 200), 1)

  # bad input
  expect_error(split_licor_file("nonexistent-file"))
  expect_error(split_licor_file(tf, split_lines = 0))
  expect_error(split_licor_file(tf, out_dir = "nonexistent-dir"))
})

test_that("fractional_doy", {
  y <- 2001
  d <- 100.5

  x <- fractional_doy(y, d)
  expect_is(x, "character")

  # Halfway through the day should be 12 o'clock
  expect_true(grepl("12:00:00", fractional_doy(y, 105.5),  fixed = TRUE))

  # The initial DOY in the return string should be the whole part of what we sent
  for(d in c(1.0, 2.12, 100, 311.2345)) {
    x <- fractional_doy(y, d)
    dback <- strsplit(x, " ")[[1]][1]
    expect_identical(as.integer(dback), as.integer(d))
  }

  # Bad input
  expect_error(fractional_doy(y, 0))
  expect_error(fractional_doy(y, 367))
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
  all_data <- list(ds1 = ds1)
  td <- tempdir()

  # Error - data subdirectory doesn't exist
  expect_error(csr_standardize_data(all_data, td, create_dirs = FALSE),
               regexp = "does not exist")

  csr_standardize_data(all_data, td, create_dirs = TRUE)
  td_dataset <- file.path(td, "ds1", "data")

  # Subdirectory was created
  expect_true(dir.exists(td_dataset))
  # Files exist
  expect_true(file.exists(file.path(td_dataset, "diag_ds1.RDS")))
  expect_true(file.exists(file.path(td_dataset, "data_ds1.RDS")))
  written_data <- readRDS(file.path(td_dataset, "data_ds1.RDS"))
  expect_identical(all_data$ds1$data, written_data)

  # Removing standardized files works
  csr_remove_stan_data(td, datasets = "ds1")
  expect_identical(list.files(td_dataset), character(0))

  # Handles no-data dataset: should create directory but write no files
  ds2 <- list(description = tibble(CSR_DATASET = "ds2"))
  all_data <- list(ds2 = ds2)

  csr_standardize_data(all_data, td, create_dirs = TRUE)
  td_dataset <- file.path(td, "ds2", "data")
  expect_true(dir.exists(td_dataset))
  expect_identical(length(list.files(td_dataset)), 0L)

  csr_remove_stan_data(td, datasets = "ds2")
  expect_identical(list.files(td_dataset), character(0))
})
