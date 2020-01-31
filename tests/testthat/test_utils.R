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

  # Handles no-data dataset: should create directory but write no files
  ds2 <- list(description = tibble(CSR_DATASET = "ds2"))

  csr_standardize_data(ds2, td, create_dirs = TRUE)
  td_dataset <- file.path(td, "ds2", "data")
  expect_true(dir.exists(td_dataset))
  expect_identical(length(list.files(td_dataset)), 0L)

  csr_remove_stan_data(td, datasets = "ds2")
  expect_identical(list.files(td_dataset), character(0))
})

test_that("convert_and_qc_timestamp", {
  # Handles bad input
  expect_error(convert_and_qc_timestamp(1, "1", "1"))
  expect_error(convert_and_qc_timestamp("1", 1, "1"))
  expect_error(convert_and_qc_timestamp("1", "1", 1))

  ts <- c("2020-01-01 12:34:56", "2021-01-01 12:34:56")
  out <- convert_and_qc_timestamp(ts, "%Y-%m-%d %H:%M:%S", "UTC")
  expect_type(out, "list")
  expect_identical(length(out), 3L)
  expect_identical(length(out$new_ts), length(ts))

  # Behaves as expected with invalid format string
  out <- convert_and_qc_timestamp(ts, "not-a-valid-format-string", "UTC")
  expect_true(all(is.na(out$new_ts)))
  expect_true(all(out$na_ts))
})


test_that("compute_interval", {
  # Bad input
  expect_error(compute_interval(1))

  x <- tibble(CSR_TIMESTAMP_BEGIN = seq(ISOdate(2000, 1, 1), by = "day", length.out = 12),
              CSR_PORT = 0)
  y <- compute_interval(x)
  expect_s3_class(y, "data.frame")
  expect_identical(y$Interval, 60 * 24)  # minutes in a day
  expect_identical(y$N, nrow(x))

  # Sorting shouldn't matter
  xrev <- x[order(x$CSR_TIMESTAMP_BEGIN, decreasing = TRUE),]
  expect_identical(compute_interval(x), compute_interval(xrev))
})

test_that("csr_build", {
  # handles bad input
  expect_error(csr_build(dataset_names = 1))
  expect_error(csr_build(force_raw = 1))
  expect_error(csr_build(write_standardized = 1))
  expect_error(csr_build(standardized_path = 1))
  expect_error(csr_build(quiet = 1))

  # not sure how to test this effectively at the moment
})
