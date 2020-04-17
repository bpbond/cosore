# test-qaqc.R

context("qaqc")

test_that("qaqc_dataset", {
  dsd <- tibble(CSR_FLUX_CO2 = c(-100, 0, 1, 100),
                CSR_T5 = c(-1, 0, 100, 0))
  diag <- tibble(X = 1)

  # Handles bad input
  expect_error(qaqc_data(dsd = 1, diag))
  expect_error(qaqc_data(dsd, diag = 1))
  expect_error(qaqc_data(dsd, diag, remove_na = 1))
  expect_error(qaqc_data(dsd, diag, remove_error = 1))

  fl <- c(-10, 50)
  tl <- c(-50, 60)

  inside_fl <- sum(dsd$CSR_FLUX_CO2 >= min(fl) & dsd$CSR_FLUX_CO2 <= max(fl))
  outside_tl <- sum(dsd$CSR_T5 < min(tl) | dsd$CSR_T5 > max(tl))

  # No flags, no change
  x <- qaqc_data(dsd, diag, remove_na = FALSE, remove_error = FALSE)
  expect_type(x, "list")
  expect_identical(length(x), 2L)
  expect_identical(x$dsd, dsd)

  # Removes errors and NAs
  dsd$CSR_FLUX_CO2[1] <- NA
  x <- qaqc_data(dsd, diag, remove_na = TRUE)
  expect_equal(nrow(x$dsd), nrow(dsd) - 1)
  expect_equal(x$diag$CSR_RECORDS_REMOVED_NA, 1)

  dsd$CSR_ERROR <- FALSE
  dsd$CSR_ERROR[1] <- TRUE
  x <- qaqc_data(dsd, diag, remove_na = FALSE, remove_error = TRUE)
  expect_equal(nrow(x$dsd), nrow(dsd) - 1)
  expect_equal(x$diag$CSR_RECORDS_REMOVED_ERR, 1)
})
