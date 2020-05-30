# test_internal_data.R

context("internal-data")

test_that("internal-data", {
  expect_error(csr_database(1))

  # Make sure that the internal COSORE_SUMMARY object has been updated
  # NB this won't catch changes to the dataset, e.g. the number of data records

  x <- csr_database(regenerate = FALSE)
  expect_identical(sort(x$CSR_DATASET), sort(list_datasets()))
})
